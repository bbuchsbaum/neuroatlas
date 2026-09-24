template_transform_identity <- function(space = "MNI152NLin6Asym") {
  plan <- structure(list(
    from_space = space, to_space = space, status = "available",
    confidence = "exact", steps = data.frame(), n_steps = 1L
  ), class = c("atlas_transform_plan", "list"))
  structure(list(
    from_space = space, to_space = space, plan = plan,
    files = NA_character_, morphism = neurotransform::IdentityMorphism(space),
    cache_dir = tempfile("transform-cache-"), engine_version = "test"
  ), class = c("template_transform", "list"))
}


template_transform_fixture_route <- function(from = "A", to = "B",
                                             artifact_id = "itk_affine") {
  fixture <- system.file("extdata", "itk_oracle", "affine.h5",
                         package = "neurotransform")
  data.frame(
    from_space = from, to_space = to, transform_type = "nonlinear",
    backend = "ants", confidence = "high", reversible = FALSE,
    data_files = NA_character_, status = "available", notes = "ITK oracle",
    artifact_id = artifact_id, artifact_version = "itk-oracle-v1",
    provider = "neuroatlas",
    url = paste0("https://example.org/releases/itk-oracle-v1/", artifact_id, ".h5"),
    sha256 = digest::digest(file = fixture, algo = "sha256", serialize = FALSE),
    size_bytes = file.info(fixture)$size, format = "ants_h5",
    qualification = "passed", qa_url = "https://example.org/qa/itk-oracle-v1",
    license = "CC0", convention = "ants_image_pullback_ras",
    stringsAsFactors = FALSE
  )
}


template_transform_volume <- function(values, space = "MNI152NLin6Asym",
                                      suffix = "T1w") {
  grid <- neuroim2::NeuroSpace(dim(values), spacing = c(1, 1, 1))
  x <- neuroim2::NeuroVol(values, grid)
  .attach_template_metadata(x, "synthetic.nii.gz", space,
                            query = list(suffix = suffix), description = list())
}


test_that("get_template_transform loads a verified native ANTs H5 morphism", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("neurotransform")
  fixture <- system.file("extdata", "itk_oracle", "affine.h5",
                         package = "neurotransform")
  skip_if(!nzchar(fixture))
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  route <- template_transform_fixture_route()
  local_mocked_bindings(
    .space_transform_registry = function() route,
    .neuroatlas_download = function(url, dest, ...) {
      expect_true(file.copy(fixture, dest))
      dest
    },
    .package = "neuroatlas"
  )

  transform <- get_template_transform("A", "B", cache_dir = cache)
  point_ras <- matrix(c(-12.29286110052785, 3.8004842688950893, 10.14), nrow = 1)
  expected_ras <- matrix(c(-12.343640351068753, 3.94042949635444,
                           10.21133859064812), nrow = 1)
  expect_s3_class(transform, "template_transform")
  expect_true(file.exists(transform$files[[1L]]))
  expect_equal(neurotransform::transform_coords(transform$morphism, point_ras),
               expected_ras, tolerance = 1e-8)
})


test_that("application rejects a transform artifact corrupted after resolution", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("neurotransform")
  fixture <- system.file("extdata", "itk_oracle", "affine.h5",
                         package = "neurotransform")
  skip_if(!nzchar(fixture))
  cache <- tempfile("transform-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  route <- template_transform_fixture_route()
  local_mocked_bindings(
    .space_transform_registry = function() route,
    .neuroatlas_download = function(url, dest, ...) {
      file.copy(fixture, dest)
      dest
    },
    .package = "neuroatlas"
  )
  transform <- get_template_transform("A", "B", cache_dir = cache)
  writeBin(charToRaw("corrupted after resolution"), transform$files[[1L]])
  source <- template_transform_volume(array(seq_len(64), c(4, 4, 4)), "A")
  target <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))

  expect_error(apply_template_transform(source, transform, target,
                                        data_type = "continuous"),
               "integrity")
})


test_that("runtime resolution rejects planned and retired artifacts", {
  skip_if_not_installed("neurotransform")
  route <- template_transform_fixture_route()
  local_mocked_bindings(.space_transform_registry = function() route,
                        .package = "neuroatlas")
  for (status in c("planned", "retired")) {
    route$status <- status
    expect_error(get_template_transform("A", "B", download = FALSE),
                 "No transform route")
  }
})


test_that("identity application uses analytic nearest labels and preserves IDs", {
  skip_if_not_installed("neurotransform")
  grid <- neuroim2::NeuroSpace(c(5, 5, 5), spacing = c(1, 1, 1))
  values <- array(0, c(5, 5, 5))
  values[2, 3, 3] <- 1
  values[4, 3, 3] <- 2
  atlas <- new_atlas(
    "labels", neuroim2::NeuroVol(values, grid), ids = 1:2,
    labels = c("one", "two"), orig_labels = c("one-original", "two-original"),
    hemi = c("left", "right"), cmap = matrix(1:6, 2),
    ref = new_atlas_ref("toy", "labels", template_space = "MNI152NLin6Asym",
                        coord_space = "MNI152", confidence = "high"),
    artifacts = .new_atlas_artifact("labels", "toy", "labels"),
    history = .new_atlas_history("load", "volume")
  )

  result <- apply_template_transform(atlas, template_transform_identity(), grid)
  expect_equal(as.vector(result$atlas), as.vector(values))
  expect_identical(result$ids, atlas$ids)
  expect_identical(result$labels, atlas$labels)
  expect_identical(result$orig_labels, atlas$orig_labels)
  expect_identical(tail(atlas_history(result)$action, 1L), "template_transform")
  expect_identical(attr(result, "neuroatlas_transform")$interpolation, "nearest")
})


test_that("probability channels are sampled once without renormalisation", {
  skip_if_not_installed("neurotransform")
  grid <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))
  probability <- neuroim2::DenseNeuroVec(
    cbind(rep(0.2, 64), rep(0.8, 64)), grid
  )
  probability <- .attach_template_metadata(
    probability, "synthetic.nii.gz", "MNI152NLin6Asym",
    query = list(suffix = "probseg"), description = list()
  )

  result <- apply_template_transform(
    probability, template_transform_identity(), grid, data_type = "probability"
  )
  expect_equal(as.vector(result), as.vector(probability))
  expect_identical(attr(result, "neuroatlas_transform")$renormalized, FALSE)
  expect_equal(as.array(result)[1, 1, 1, ], c(0.2, 0.8))
})


test_that("application rejects unknown data semantics and mismatched identities", {
  skip_if_not_installed("neurotransform")
  grid <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))
  bare <- neuroim2::NeuroVol(array(1, c(4, 4, 4)), grid)
  transform <- template_transform_identity()
  expect_error(apply_template_transform(bare, transform, grid), "Unannotated")
  expect_error(apply_template_transform(bare, transform, grid,
                                        data_type = "not-a-type"),
               "should be one of")

  source <- template_transform_volume(array(1, c(4, 4, 4)), "Wrong")
  expect_error(apply_template_transform(source, transform, grid,
                                        data_type = "continuous"),
               "Declared source template does not match")
  target <- template_transform_volume(array(1, c(4, 4, 4)), "Wrong")
  source <- template_transform_volume(array(1, c(4, 4, 4)))
  expect_error(apply_template_transform(source, transform, target,
                                        data_type = "continuous"),
               "Declared target template does not match")
})


test_that("a multi-step route composes pullbacks once in physical coordinates", {
  skip_if_not_installed("neurotransform")
  route <- rbind(
    transform_route <- data.frame(
      from_space = "A", to_space = "B", transform_type = "affine",
      backend = "internal_affine", confidence = "exact", reversible = TRUE,
      data_files = NA_character_, status = "available", notes = "",
      artifact_id = "a-b", artifact_version = NA_character_,
      provider = "neuroatlas", url = NA_character_, sha256 = NA_character_,
      size_bytes = NA_real_, format = NA_character_, qualification = NA_character_,
      qa_url = NA_character_, license = NA_character_, convention = NA_character_,
      stringsAsFactors = FALSE
    ),
    transform_route
  )
  route$from_space[[2L]] <- "B"
  route$to_space[[2L]] <- "C"
  route$artifact_id[[2L]] <- "b-c"
  forward_a_b <- matrix(c(0, 1, 0, 0,
                          -1, 0, 0, 0,
                          0, 0, 1, 0,
                          3, 2, 0, 1), 4, 4)
  forward_b_c <- diag(4)
  forward_b_c[1:3, 4] <- c(5, -1, 0)
  local_mocked_bindings(
    .space_transform_registry = function() route,
    get_space_transform = function(from, to) {
      if (identical(from, "A") && identical(to, "B")) forward_a_b else forward_b_c
    },
    .package = "neuroatlas"
  )

  transform <- get_template_transform("A", "C", download = FALSE)
  point_c <- matrix(c(7, 4, 0), nrow = 1)
  expected_a <- t(solve(forward_a_b) %*% solve(forward_b_c) %*%
                    c(point_c[1, ], 1))[, 1:3, drop = FALSE]
  expect_identical(transform$plan$n_steps, 2L)
  expect_equal(neurotransform::transform_coords(transform$morphism, point_c),
               expected_a, tolerance = 1e-12)
})


test_that("clustered atlas backing preserves noncontiguous semantic IDs and loss", {
  skip_if_not_installed("neurotransform")
  source_grid <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))
  source_values <- array(0, c(4, 4, 4))
  source_values[1, 1, 1] <- 17
  source_values[4, 4, 4] <- 99
  source_vol <- neuroim2::NeuroVol(source_values, source_grid)
  clustered <- neuroim2::ClusteredNeuroVol(
    as.logical(source_vol), clusters = source_vol[source_vol != 0]
  )
  atlas <- new_atlas(
    "noncontiguous", clustered, ids = c(17L, 99L),
    labels = c("seventeen", "ninety-nine"),
    hemi = c("left", "right"), cmap = matrix(1:6, 2),
    ref = new_atlas_ref("toy", "noncontiguous",
                        template_space = "MNI152NLin6Asym",
                        coord_space = "MNI152", confidence = "high"),
    artifacts = .new_atlas_artifact("labels", "toy", "noncontiguous"),
    history = .new_atlas_history("load", "volume")
  )
  target_grid <- neuroim2::NeuroSpace(c(2, 2, 2), spacing = c(1, 1, 1))

  result <- apply_template_transform(atlas, template_transform_identity(), target_grid)
  expect_identical(result$ids, c(17L, 99L))
  expect_identical(result$labels, atlas$labels)
  expect_equal(unique(as.vector(result$atlas)), c(17, 0))
  expect_identical(attr(result, "neuroatlas_transform")$lost_label_ids, 99)
})


test_that("bare continuous inputs acquire asserted metadata for later checks", {
  skip_if_not_installed("neurotransform")
  grid <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))
  bare <- neuroim2::NeuroVol(array(seq_len(64), c(4, 4, 4)), grid)
  result <- apply_template_transform(
    bare, template_transform_identity(), grid, data_type = "continuous"
  )
  metadata <- attr(result, "neuroatlas_metadata", exact = TRUE)
  expect_s3_class(metadata, "NeuroResourceMetadata")
  expect_identical(metadata$spatial$template_space, "MNI152NLin6Asym")
  expect_identical(metadata$spatial$basis, "user_supplied")
  expect_identical(metadata$parents$source$spatial$basis, "user_supplied")

  wrong_source <- get_template_transform("MNI305", "MNI152",
                                         provider = "neuroatlas")
  expect_error(apply_template_transform(result, wrong_source, grid,
                                        data_type = "continuous"),
               "source template does not match")
})


test_that("logical mask volumes are valid label inputs", {
  skip_if_not_installed("neurotransform")
  grid <- neuroim2::NeuroSpace(c(4, 4, 4), spacing = c(1, 1, 1))
  mask <- neuroim2::LogicalNeuroVol(
    array(rep(c(FALSE, TRUE), length.out = 64), c(4, 4, 4)), grid
  )
  result <- apply_template_transform(mask, template_transform_identity(), grid,
                                     data_type = "label")
  expect_equal(as.vector(result), as.numeric(as.vector(mask)))
  expect_identical(attr(result, "neuroatlas_transform")$interpolation, "nearest")
})


test_that("built-in neuroatlas affine route remains provider-selectable", {
  skip_if_not_installed("neurotransform")
  transform <- get_template_transform("MNI305", "MNI152", provider = "neuroatlas")
  expect_identical(transform$plan$status, "available")
  expect_identical(transform$plan$steps$backend[[1L]], "internal_affine")
  expect_identical(transform$files[[1L]], NA_character_)
})

test_that("a resolved transform survives working-directory changes", {
  skip_if_not_installed("neurotransform")
  fixture <- system.file("extdata", "itk_oracle", "affine.h5",
                         package = "neurotransform")
  skip_if(!nzchar(fixture))
  root <- tempfile("relative-transform-")
  dir.create(root)
  old_dir <- getwd()
  on.exit({ setwd(old_dir); unlink(root, recursive = TRUE) }, add = TRUE)
  setwd(root)
  route <- template_transform_fixture_route()
  local_mocked_bindings(
    .space_transform_registry = function() route,
    .neuroatlas_download = function(url, dest, ...) file.copy(fixture, dest),
    .package = "neuroatlas"
  )
  transform <- get_template_transform("A", "B", cache_dir = "relative-cache")
  expect_identical(transform$cache_dir, file.path(normalizePath(root), "relative-cache"))
  setwd(old_dir)
  source <- template_transform_volume(array(seq_len(64), c(4, 4, 4)), "A")
  target <- neuroim2::NeuroSpace(c(4, 4, 4))
  expect_s4_class(apply_template_transform(source, transform, target), "NeuroVol")
})

test_that("engine H5 semantics satisfy independent SimpleITK probes", {
  skip_if_not_installed("neurotransform")
  expect_silent(neuroatlas:::.check_neurotransform_semantics())
})

test_that("cache clear is excluded while a lazy warp becomes a sampling plan", {
  skip_if_not_installed("neurotransform")
  fixture <- system.file("extdata", "itk_oracle", "affine_warp.h5",
                         package = "neurotransform")
  skip_if(!nzchar(fixture))
  cache <- tempfile("active-warp-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  route <- template_transform_fixture_route()
  route$sha256 <- digest::digest(file = fixture, algo = "sha256", serialize = FALSE)
  route$size_bytes <- file.info(fixture)$size
  original_fetch <- neuroatlas:::.fetch_transform_artifact_unlocked
  local_mocked_bindings(
    .space_transform_registry = function() route,
    .neuroatlas_download = function(url, dest, ...) file.copy(fixture, dest),
    .package = "neuroatlas"
  )
  transform <- get_template_transform("A", "B", cache_dir = cache)
  withr::local_options(neuroatlas.transform_cache_lock_timeout = 0)
  local_mocked_bindings(.fetch_transform_artifact_unlocked = function(...) {
    path <- original_fetch(...)
    expect_error(clear_transform_cache(cache_dir = cache), "Timed out")
    path
  }, .package = "neuroatlas")
  grid <- neuroim2::NeuroSpace(c(4, 4, 4), origin = c(-15, 0, 9))
  source <- neuroim2::NeuroVol(array(1, c(4, 4, 4)), grid)
  expect_s4_class(apply_template_transform(source, transform, grid,
    data_type = "continuous"), "NeuroVol")
})


test_that("missing HDF5 support has an actionable dependency error", {
  check <- .require_neurotransform
  environment(check) <- list2env(list(
    requireNamespace = function(package, ...) package != "hdf5r"
  ), parent = environment(.require_neurotransform))
  expect_error(check(), "optional 'hdf5r'.*Install it")
})

schaefer_test_labels <- function(parcels, networks) {
  hemi <- rep(c("LH", "RH"), each = parcels / 2L)
  network <- rep(paste0("Net", seq_len(as.integer(networks))),
                 length.out = parcels)
  data.frame(
    roinum = seq_len(parcels),
    label = sprintf(
      "%sNetworks_%s_%s_%d", networks, hemi, network, seq_len(parcels)
    ),
    red = rep(100L, parcels),
    green = rep(150L, parcels),
    blue = rep(200L, parcels)
  )
}


test_that("Schaefer labels use an explicit cache and preserve source receipts", {
  cache_dir <- tempfile("neuroatlas-schaefer-cache-")
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L

  testthat::local_mocked_bindings(
    .neuroatlas_download = function(url, dest, ...) {
      calls$n <- calls$n + 1L
      utils::write.table(
        schaefer_test_labels(400L, "17"), dest,
        row.names = FALSE, col.names = FALSE, quote = FALSE
      )
      dest
    },
    .package = "neuroatlas"
  )

  labels <- neuroatlas:::load_schaefer_labels(
    "400", "17", use_cache = TRUE, cache_dir = cache_dir
  )
  cache_file <- file.path(
    cache_dir, "Schaefer2018_400Parcels_17Networks_order.txt"
  )
  expect_equal(calls$n, 1L)
  expect_true(file.exists(cache_file))
  expect_identical(
    attr(labels, "neuroatlas_source")$storage,
    "transient_download_materialized_to_configured_cache"
  )
  expect_true(nzchar(attr(labels, "neuroatlas_file_receipt")$checksum))

  testthat::local_mocked_bindings(
    .neuroatlas_download = function(...) stop("unexpected download"),
    .package = "neuroatlas"
  )
  cached_labels <- neuroatlas:::load_schaefer_labels(
    "400", "17", use_cache = TRUE, cache_dir = cache_dir
  )
  expect_identical(
    attr(cached_labels, "neuroatlas_source")$storage,
    "configured_cache"
  )
  expect_identical(
    attr(cached_labels, "neuroatlas_file_receipt")$local_path,
    normalizePath(cache_file)
  )
})


test_that("Schaefer transient loads do not materialise a default cache", {
  cache_dir <- tempfile("neuroatlas-schaefer-cache-")
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L

  testthat::local_mocked_bindings(
    .neuroatlas_cache_dir = function(...) stop("unexpected cache lookup"),
    .neuroatlas_download = function(url, dest, ...) {
      calls$n <- calls$n + 1L
      utils::write.table(
        schaefer_test_labels(400L, "17"), dest,
        row.names = FALSE, col.names = FALSE, quote = FALSE
      )
      dest
    },
    .package = "neuroatlas"
  )

  labels <- neuroatlas:::load_schaefer_labels(
    "400", "17", use_cache = FALSE, cache_dir = cache_dir
  )
  expect_equal(calls$n, 1L)
  expect_false(dir.exists(cache_dir))
  expect_identical(
    attr(labels, "neuroatlas_source")$storage,
    "transient_download"
  )
  expect_true(file.exists(attr(labels, "neuroatlas_file_receipt")$local_path))
})


test_that("Schaefer volumes use configured or transient storage as requested", {
  source_file <- system.file("extdata", "atlas_aparc_aseg_prob33.nii.gz",
                             package = "neuroatlas")
  cache_dir <- tempfile("neuroatlas-schaefer-cache-")
  dir.create(cache_dir)
  cache_file <- file.path(
    cache_dir, "Schaefer2018_400Parcels_17Networks_order_FSLMNI152_2mm.nii.gz"
  )
  expect_true(file.copy(source_file, cache_file))

  testthat::local_mocked_bindings(
    .neuroatlas_download = function(...) stop("unexpected download"),
    .package = "neuroatlas"
  )
  cached <- neuroatlas:::load_schaefer_vol(
    "400", "17", "2", use_cache = TRUE, cache_dir = cache_dir
  )
  expect_s4_class(cached, "NeuroVol")
  expect_identical(
    attr(cached, "neuroatlas_source")$storage, "configured_cache"
  )
  expect_identical(
    attr(cached, "neuroatlas_file_receipt")$local_path,
    normalizePath(cache_file)
  )

  transient_dir <- tempfile("neuroatlas-schaefer-cache-")
  testthat::local_mocked_bindings(
    .neuroatlas_cache_dir = function(...) stop("unexpected cache lookup"),
    .neuroatlas_download = function(url, dest, ...) {
      expect_true(file.copy(source_file, dest))
      dest
    },
    .package = "neuroatlas"
  )
  transient <- neuroatlas:::load_schaefer_vol(
    "400", "17", "2", use_cache = FALSE, cache_dir = transient_dir
  )
  expect_s4_class(transient, "NeuroVol")
  expect_false(dir.exists(transient_dir))
  expect_identical(
    attr(transient, "neuroatlas_source")$storage, "transient_download"
  )
  expect_true(file.exists(attr(transient, "neuroatlas_file_receipt")$local_path))
})


test_that("Schaefer 400/17 wrapper forwards the configured cache", {
  cache_dir <- tempfile("neuroatlas-schaefer-cache-")
  calls <- new.env(parent = emptyenv())
  calls$volume_cache <- NULL
  calls$label_cache <- NULL
  source_space <- neuroim2::NeuroSpace(
    dim = c(20L, 20L, 1L), spacing = c(1, 1, 1), origin = c(0, 0, 0)
  )
  source_vol <- neuroim2::NeuroVol(
    array(seq_len(400L), dim = c(20L, 20L, 1L)), source_space
  )

  testthat::local_mocked_bindings(
    load_schaefer_vol = function(parcels, networks, resolution, use_cache,
                                 cache_dir) {
      calls$volume_cache <- cache_dir
      source_vol
    },
    schaefer_metainfo = function(parcels, networks, use_cache, cache_dir) {
      calls$label_cache <- cache_dir
      labels <- schaefer_test_labels(400L, "17")
      labels$hemi <- ifelse(labels$label |> substr(12L, 13L) == "LH", "left", "right")
      labels$network <- rep(paste0("Net", seq_len(17L)), length.out = 400L)
      labels$name <- paste0(labels$network, "_", labels$roinum)
      labels
    },
    .package = "neuroatlas"
  )

  atlas <- sy_400_17(cache_dir = cache_dir)
  expect_identical(calls$volume_cache, cache_dir)
  expect_identical(calls$label_cache, cache_dir)
  expect_length(atlas$ids, 400L)
  expect_equal(length(unique(atlas$network)), 17L)
})


test_that("Schaefer rejects a named target with only a planned warp", {
  plan <- atlas_transform_plan("MNI152NLin6Asym", "MNI152NLin2009cAsym")
  plan$status <- "planned"
  plan$steps$status[] <- "planned"
  calls <- new.env(parent = emptyenv())
  calls$resolve <- 0L
  calls$load <- 0L
  target_space <- neuroim2::NeuroSpace(
    dim = c(20L, 20L, 20L), spacing = c(1, 1, 1), origin = c(0, 0, 0)
  )

  testthat::local_mocked_bindings(
    atlas_transform_plan = function(...) plan,
    .resolve_template_input = function(input, target_type) {
      calls$resolve <- calls$resolve + 1L
      target_space
    },
    load_schaefer_vol = function(...) {
      calls$load <- calls$load + 1L
      stop("source volume must not be loaded")
    },
    .package = "neuroatlas"
  )

  expect_error(
    get_schaefer_atlas(
      parcels = "400", networks = "17", outspace = "MNI152NLin2009cAsym"
    ),
    "required transform is planned or unavailable"
  )
  expect_equal(calls$resolve, 1L)
  expect_equal(calls$load, 0L)
})


test_that("Schaefer rejects an available route without an execution backend", {
  target_space <- neuroim2::NeuroSpace(
    dim = c(20L, 20L, 20L), spacing = c(1, 1, 1), origin = c(0, 0, 0)
  )
  plan <- structure(
    list(
      from_space = "MNI152NLin6Asym",
      to_space = "MNI152",
      status = "available",
      steps = data.frame(transform_type = "affine", stringsAsFactors = FALSE)
    ),
    class = c("atlas_transform_plan", "list")
  )

  testthat::local_mocked_bindings(
    .resolve_template_input = function(input, target_type) target_space,
    atlas_transform_plan = function(...) plan,
    .package = "neuroatlas"
  )

  expect_error(
    get_schaefer_atlas(parcels = "400", networks = "17", outspace = "MNI152"),
    "no registered execution backend"
  )
})


test_that("neurosurf is optional at package load", {
  skip_if_not(file.exists(testthat::test_path("..", "..", "DESCRIPTION")),
              "source package metadata check")
  description <- read.dcf(testthat::test_path("..", "..", "DESCRIPTION"))
  imports <- trimws(strsplit(description[1, "Imports"], ",")[[1]])
  suggests <- trimws(strsplit(description[1, "Suggests"], ",")[[1]])
  namespace <- readLines(testthat::test_path("..", "..", "NAMESPACE"), warn = FALSE)

  expect_false(any(grepl("^neurosurf", imports)))
  expect_true(any(grepl("^neurosurf", suggests)))
  expect_false(any(grepl("^importFrom\\(neurosurf", namespace)))
})

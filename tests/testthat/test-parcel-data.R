make_toy_parcel_atlas <- function() {
  atlas_obj <- list(
    name = "toy_atlas",
    atlas = array(c(0L, 1L, 2L, 3L), dim = c(2, 2, 1)),
    ids = c(1L, 2L, 3L),
    labels = c("A", "B", "C"),
    orig_labels = c("lh_A", "rh_B", "lh_C"),
    hemi = c("left", "right", "left"),
    cmap = data.frame(r = c(255L, 0L, 0L),
                      g = c(0L, 255L, 0L),
                      b = c(0L, 0L, 255L))
  )
  class(atlas_obj) <- c("toy", "atlas")
  atlas_obj
}

make_toy_volumetric_parcel_atlas <- function(clustered = FALSE) {
  atlas_obj <- make_toy_parcel_atlas()
  labels <- array(
    c(0L, 1L, 1L, 2L, 2L, 3L, 3L, 0L),
    dim = c(2, 2, 2)
  )
  space <- neuroim2::NeuroSpace(
    dim = dim(labels),
    spacing = c(2, 3, 4),
    origin = c(-10, 5, 12)
  )

  if (clustered) {
    mask <- neuroim2::LogicalNeuroVol(labels != 0L, space = space)
    atlas_obj$atlas <- neuroim2::ClusteredNeuroVol(
      mask = mask,
      clusters = labels[labels != 0L]
    )
  } else {
    atlas_obj$atlas <- neuroim2::NeuroVol(labels, space = space)
  }
  atlas_obj
}

test_that("parcel_data constructor validates and stores schema", {
  tbl <- tibble::tibble(
    id = c(1L, 2L),
    label = c("A", "B"),
    hemi = c("left", "right"),
    beta = c(0.1, -0.4)
  )

  x <- parcel_data(tbl, atlas_id = "toy_v1")

  expect_s3_class(x, "parcel_data")
  expect_equal(x$atlas$id, "toy_v1")
  expect_equal(x$atlas$n_parcels, 2)
  expect_equal(names(x$parcels), c("id", "label", "hemi", "beta"))
})

test_that("parcel_data constructor never truncates fractional IDs", {
  tbl <- tibble::tibble(
    id = c(1, 2.5),
    label = c("A", "B"),
    hemi = c("left", "right")
  )

  expect_error(parcel_data(tbl, atlas_id = "toy"), "must contain integers")
})

test_that("as_parcel_data.atlas attaches vector values", {
  atlas <- make_toy_parcel_atlas()
  x <- as_parcel_data(atlas, values = c(2.0, 4.0, 6.0), value_col = "score")

  expect_s3_class(x, "parcel_data")
  expect_equal(x$atlas$id, "toy_atlas")
  expect_equal(x$parcels$id, c(1L, 2L, 3L))
  expect_equal(x$parcels$score, c(2.0, 4.0, 6.0))
})

test_that("as_parcel_data.atlas requires explicit partial coverage", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    id = c(3L, 1L),
    statistic = c(30, 10)
  )

  expect_error(
    as_parcel_data(atlas, values = values),
    "missing 1 atlas parcel",
    class = "neuroatlas_error_missing_parcel_key"
  )
  x <- as_parcel_data(atlas, values = values, allow_partial = TRUE)

  expect_equal(x$parcels$statistic, c(10, NA, 30))
})

test_that("align_parcel_values supports renamed integer keys", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    roi_index = c("3", "1", "2"),
    statistic = c(30, 10, 20)
  )

  aligned <- align_parcel_values(
    atlas,
    values,
    value = statistic,
    by = c(id = "roi_index")
  )

  expect_equal(unname(aligned), c(10, 20, 30))
  expect_equal(names(aligned), c("1", "2", "3"))
})

test_that("align_parcel_values supports strings and full-label inference", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    label_full = c("lh_C", "lh_A", "rh_B"),
    estimate = c(3, 1, 2)
  )

  aligned <- align_parcel_values(atlas, values, value = "estimate")
  expect_equal(unname(aligned), c(1, 2, 3))
})

test_that("parcel_volume aligns values and preserves atlas space", {
  atlas <- make_toy_volumetric_parcel_atlas()
  values <- tibble::tibble(
    roi_index = c(3L, 1L, 2L),
    z_stat = c(30, 10, 20)
  )

  result <- parcel_volume(
    atlas,
    values,
    value = z_stat,
    by = c(id = "roi_index")
  )
  labels <- array(as.numeric(atlas$atlas), dim = dim(atlas$atlas))
  expected <- array(NA_real_, dim = dim(labels))
  expected[labels == 1L] <- 10
  expected[labels == 2L] <- 20
  expected[labels == 3L] <- 30

  expect_s4_class(result, "NeuroVol")
  expect_equal(array(as.numeric(result), dim = dim(result)), expected)
  expect_equal(neuroim2::space(result), neuroim2::space(atlas$atlas))
})

test_that("parcel_volume distinguishes partial parcels from background", {
  atlas <- make_toy_volumetric_parcel_atlas()
  values <- tibble::tibble(
    id = c(3L, 1L),
    beta = c(0.3, 0.1)
  )

  expect_error(
    parcel_volume(atlas, values, beta),
    "missing 1 atlas parcel",
    class = "neuroatlas_error_missing_parcel_key"
  )

  result <- parcel_volume(
    atlas,
    values,
    value = "beta",
    allow_partial = TRUE,
    background = -99
  )
  labels <- array(as.numeric(atlas$atlas), dim = dim(atlas$atlas))
  observed <- array(as.numeric(result), dim = dim(result))

  expect_true(all(observed[labels == 0L] == -99))
  expect_true(all(observed[labels == 1L] == 0.1))
  expect_true(all(is.na(observed[labels == 2L])))
  expect_true(all(observed[labels == 3L] == 0.3))
})

test_that("parcel_volume supports clustered atlas storage", {
  dense_atlas <- make_toy_volumetric_parcel_atlas()
  clustered_atlas <- make_toy_volumetric_parcel_atlas(clustered = TRUE)
  values <- tibble::tibble(id = c(2L, 3L, 1L), statistic = c(2, 3, 1))

  dense <- parcel_volume(dense_atlas, values, statistic, background = 0)
  clustered <- parcel_volume(
    clustered_atlas,
    values,
    statistic,
    background = 0
  )

  expect_equal(as.numeric(clustered), as.numeric(dense))
  expect_equal(
    neuroim2::space(clustered),
    neuroim2::space(clustered_atlas$atlas)
  )
})

test_that("parcel_volume fails closed for invalid rendering inputs", {
  atlas <- make_toy_volumetric_parcel_atlas()
  values <- tibble::tibble(id = 1:3, statistic = c(1, 2, 3))

  surface_atlas <- atlas
  class(surface_atlas) <- c("toy_surface", "surfatlas", "atlas")
  expect_error(
    parcel_volume(surface_atlas, values, statistic),
    "must be a volumetric",
    class = "neuroatlas_error_parcel_volume"
  )

  expect_error(
    parcel_volume(atlas, values, statistic, background = Inf),
    "finite number or `NA_real_`",
    class = "neuroatlas_error_parcel_volume"
  )

  values$statistic[[2]] <- Inf
  expect_error(
    parcel_volume(atlas, values, statistic),
    "must not contain infinite values",
    class = "neuroatlas_error_parcel_value"
  )

  atlas$atlas <- neuroim2::NeuroVol(
    array(c(0L, 1L, 2L, 99L, 0L, 1L, 2L, 3L), dim = c(2, 2, 2)),
    space = neuroim2::space(atlas$atlas)
  )
  values$statistic[[2]] <- 2
  expect_error(
    parcel_volume(atlas, values, statistic),
    "Unknown parcel ID.*99",
    class = "neuroatlas_error_parcel_volume"
  )

  atlas$atlas <- array(1:3, dim = c(3, 1, 1))
  expect_error(
    parcel_volume(atlas, values, statistic),
    "Could not obtain a volume",
    class = "neuroatlas_error_parcel_volume"
  )
})

test_that("short labels must be unique or made composite", {
  atlas <- make_toy_parcel_atlas()
  atlas$labels <- c("insula", "insula", "visual")
  atlas$orig_labels <- c("lh_insula", "rh_insula", "lh_visual")
  atlas$hemi <- c("left", "right", "left")
  atlas$roi_metadata <- NULL

  label_only <- tibble::tibble(
    label = c("insula", "visual"),
    statistic = c(1, 2)
  )
  expect_error(
    align_parcel_values(atlas, label_only, statistic, by = "label"),
    "not unique"
  )
  expect_error(
    align_parcel_values(atlas, label_only, statistic),
    "Could not infer a safe unique parcel key"
  )

  composite <- tibble::tibble(
    label = c("visual", "insula", "insula"),
    hemi = c("left", "right", "left"),
    statistic = c(3, 2, 1)
  )
  aligned <- align_parcel_values(atlas, composite, statistic)
  expect_equal(unname(aligned), c(1, 2, 3))
})

test_that("alignment rejects duplicate, unknown, and non-integer keys", {
  atlas <- make_toy_parcel_atlas()

  duplicate <- tibble::tibble(
    id = c(1L, 1L, 3L),
    statistic = 1:3
  )
  expect_error(
    align_parcel_values(atlas, duplicate, statistic),
    "identify each row uniquely",
    class = "neuroatlas_error_duplicate_parcel_key"
  )

  unknown <- tibble::tibble(
    id = c(1L, 2L, 99L),
    statistic = 1:3
  )
  expect_error(
    align_parcel_values(atlas, unknown, statistic),
    "did not match the atlas",
    class = "neuroatlas_error_unknown_parcel_key"
  )

  fractional <- tibble::tibble(
    id = c(1, 2, 3.5),
    statistic = 1:3
  )
  expect_error(
    align_parcel_values(atlas, fractional, statistic),
    "must contain integers"
  )

  empty <- tibble::tibble(id = integer(), statistic = numeric())
  expect_error(
    align_parcel_values(
      atlas,
      empty,
      statistic,
      allow_partial = TRUE
    ),
    "at least one parcel row"
  )
})

test_that("alignment rejects inconsistent canonical atlas IDs", {
  atlas <- make_toy_parcel_atlas()
  atlas$roi_metadata <- roi_metadata(atlas)
  atlas$roi_metadata$id <- c(1L, 3L, 2L)
  values <- tibble::tibble(id = 1:3, statistic = 1:3)

  expect_error(
    align_parcel_values(atlas, values, statistic),
    "inconsistent parcel IDs"
  )
})

test_that("canonical metadata verifies but never overwrites the atlas", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    id = c(3L, 1L, 2L),
    label = c("C", "A", "B"),
    statistic = c(30, 10, 20)
  )

  x <- as_parcel_data(atlas, values = values)
  expect_equal(x$parcels$label, atlas$labels)
  expect_equal(x$parcels$statistic, c(10, 20, 30))

  values$label[[1]] <- "wrong"
  expect_error(
    as_parcel_data(atlas, values = values),
    "disagrees with canonical atlas metadata"
  )
})

test_that("alignment requires a numeric selected value", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    id = 1:3,
    description = c("one", "two", "three")
  )

  expect_error(
    align_parcel_values(atlas, values, description),
    "numeric vector"
  )
  expect_error(
    align_parcel_values(atlas, values, absent),
    "was not found"
  )

  matrix_values <- tibble::tibble(id = 1:3)
  matrix_values$statistic <- matrix(1:6, nrow = 3)
  expect_error(
    align_parcel_values(atlas, matrix_values, statistic),
    "must be a numeric vector"
  )
})

test_that("vector attachment cannot overwrite canonical metadata", {
  atlas <- make_toy_parcel_atlas()
  expect_error(
    as_parcel_data(atlas, values = 1:3, value_col = "id"),
    "cannot overwrite atlas metadata"
  )
})

test_that("parcel_values returns values aligned to atlas ids", {
  atlas <- make_toy_parcel_atlas()

  x <- parcel_data(
    parcels = tibble::tibble(
      id = c(3L, 1L),
      label = c("C", "A"),
      hemi = c("left", "left"),
      statistic = c(30, 10)
    ),
    atlas_id = "toy_v1"
  )

  vals <- parcel_values(x, atlas, column = "statistic")
  expect_equal(vals, c(10, NA, 30))

  x$parcels$label[[1]] <- "wrong"
  expect_error(
    parcel_values(x, atlas, column = "statistic"),
    "disagrees with canonical atlas metadata"
  )
})

test_that("as_parcel_data accepts a compatible parcel_data table", {
  atlas <- make_toy_parcel_atlas()
  source <- parcel_data(
    parcels = tibble::tibble(
      id = c(3L, 1L, 2L),
      label = c("C", "A", "B"),
      hemi = c("left", "left", "right"),
      statistic = c(30, 10, 20)
    ),
    atlas_id = "toy_atlas"
  )

  result <- as_parcel_data(atlas, values = source)
  expect_equal(result$parcels$statistic, c(10, 20, 30))
})

test_that("write/read parcel_data roundtrip for rds", {
  atlas <- make_toy_parcel_atlas()
  x <- as_parcel_data(atlas, values = c(1.1, 2.2, 3.3), value_col = "beta")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)

  write_parcel_data(x, path)
  y <- read_parcel_data(path)

  expect_s3_class(y, "parcel_data")
  expect_equal(y$atlas$id, x$atlas$id)
  expect_equal(y$parcels$beta, x$parcels$beta)
})

test_that("write/read parcel_data roundtrip for json", {
  skip_if_not_installed("jsonlite")

  atlas <- make_toy_parcel_atlas()
  x <- as_parcel_data(atlas, values = c(1.1, 2.2, 3.3), value_col = "beta")

  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  write_parcel_data(x, path)
  y <- read_parcel_data(path)

  expect_s3_class(y, "parcel_data")
  expect_equal(y$atlas$id, x$atlas$id)
  expect_equal(y$parcels$beta, x$parcels$beta)
})

test_that("validate_parcel_data errors for duplicate ids", {
  bad <- structure(
    list(
      schema_version = "1.0.0",
      atlas = list(id = "toy", n_parcels = 2),
      parcels = tibble::tibble(
        id = c(1L, 1L),
        label = c("A", "B"),
        hemi = c("left", "right")
      )
    ),
    class = c("parcel_data", "list")
  )

  expect_error(validate_parcel_data(bad), "must be unique")
})

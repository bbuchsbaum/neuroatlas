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

test_that("as_parcel_data.atlas attaches vector values", {
  atlas <- make_toy_parcel_atlas()
  x <- as_parcel_data(atlas, values = c(2.0, 4.0, 6.0), value_col = "score")

  expect_s3_class(x, "parcel_data")
  expect_equal(x$atlas$id, "toy_atlas")
  expect_equal(x$parcels$id, c(1L, 2L, 3L))
  expect_equal(x$parcels$score, c(2.0, 4.0, 6.0))
})

test_that("as_parcel_data.atlas joins value tables by id", {
  atlas <- make_toy_parcel_atlas()
  values <- tibble::tibble(
    id = c(3L, 1L),
    statistic = c(30, 10)
  )

  x <- as_parcel_data(atlas, values = values)

  expect_equal(x$parcels$statistic, c(10, NA, 30))
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

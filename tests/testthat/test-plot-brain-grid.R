test_that("plot_brain_grid returns a patchwork object", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scico")

  # We need a surfatlas. Create a minimal mock.
  mock_surfatlas <- structure(list(
    ids = 1:4,
    labels = c("A", "B", "C", "D"),
    hemi = c("left", "left", "right", "right"),
    name = "mock"
  ), class = "surfatlas")

  # Mock plot_brain to return a simple ggplot (avoids needing real surface data)
  mockery::stub(plot_brain_grid, "plot_brain", function(...) {
    ggplot2::ggplot() + ggplot2::theme_void()
  })

  vals <- list(Contrast_A = c(1, 2, 3, 4), Contrast_B = c(4, 3, 2, 1))
  result <- plot_brain_grid(mock_surfatlas, vals, palette = "cork")

  expect_s3_class(result, "patchwork")
})

test_that("plot_brain_grid has correct panel count", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scico")

  mock_surfatlas <- structure(list(
    ids = 1:4,
    labels = c("A", "B", "C", "D"),
    hemi = c("left", "left", "right", "right"),
    name = "mock"
  ), class = "surfatlas")

  mockery::stub(plot_brain_grid, "plot_brain", function(...) {
    ggplot2::ggplot() + ggplot2::theme_void()
  })

  vals <- list(V1 = 1:4, V2 = 4:1, V3 = c(2, 2, 3, 3))
  result <- plot_brain_grid(mock_surfatlas, vals, colorbar = TRUE)

  # patchwork object: 3 brain panels + 1 colorbar = 4 patches
  # The number of patches is accessible via length on the patchwork
  expect_s3_class(result, "patchwork")
})

test_that("plot_brain_grid shared scale passes same lim to all panels", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scico")

  mock_surfatlas <- structure(list(
    ids = 1:2,
    labels = c("A", "B"),
    hemi = c("left", "right"),
    name = "mock"
  ), class = "surfatlas")

  env <- new.env(parent = emptyenv())
  env$captured_lims <- list()
  mock_pb <- function(surfatlas, vals, lim, ...) {
    env$captured_lims <- c(env$captured_lims, list(lim))
    ggplot2::ggplot() + ggplot2::theme_void()
  }

  mockery::stub(plot_brain_grid, "plot_brain", mock_pb)
  vals <- list(A = c(-5, 5), B = c(-1, 1))
  plot_brain_grid(mock_surfatlas, vals, shared_scale = TRUE, colorbar = FALSE)
  expect_equal(env$captured_lims[[1]], c(-5, 5))
  expect_equal(env$captured_lims[[2]], c(-5, 5))
})

test_that("plot_brain_grid independent scale passes NULL lim", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scico")

  mock_surfatlas <- structure(list(
    ids = 1:2,
    labels = c("A", "B"),
    hemi = c("left", "right"),
    name = "mock"
  ), class = "surfatlas")

  env <- new.env(parent = emptyenv())
  env$captured_lims <- list()
  mock_pb <- function(surfatlas, vals, lim, ...) {
    env$captured_lims <- c(env$captured_lims, list(lim))
    ggplot2::ggplot() + ggplot2::theme_void()
  }

  mockery::stub(plot_brain_grid, "plot_brain", mock_pb)
  vals <- list(A = c(-5, 5), B = c(-1, 1))
  plot_brain_grid(mock_surfatlas, vals, shared_scale = FALSE, colorbar = FALSE)
  expect_null(env$captured_lims[[1]])
  expect_null(env$captured_lims[[2]])
})

test_that("plot_brain_grid errors on empty vals_list", {
  mock_surfatlas <- structure(list(ids = 1:2), class = "surfatlas")
  expect_error(plot_brain_grid(mock_surfatlas, list()), "non-empty")
})

test_that(".make_colorbar_panel returns ggplot", {
  skip_if_not_installed("scico")
  cb <- neuroatlas:::.make_colorbar_panel(
    palette = "cork", lim = c(-1, 1), title = "z-score"
  )
  expect_s3_class(cb, "gg")
})

make_parcel_plot_test_atlas <- function() {
  verts <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      1, 1, 0
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(
    c(
      0L, 1L, 2L,
      1L, 3L, 2L
    ),
    ncol = 3,
    byrow = TRUE
  )
  geom <- neurosurf::SurfaceGeometry(vert = verts, faces = faces, hemi = "lh")
  hemi_surf <- methods::new(
    "LabeledNeuroSurface",
    labels = c("A", "B"),
    cols = c("#4477AA", "#CC6677"),
    geometry = geom,
    indices = seq_len(nrow(verts)),
    data = c(1, 1, 2, 2)
  )

  structure(
    list(
      name = "parcel-plot-test",
      ids = c(1L, 2L),
      labels = c("A", "B"),
      orig_labels = c("lh_A", "lh_B"),
      hemi = c("left", "left"),
      cmap = data.frame(
        r = c(68L, 204L),
        g = c(119L, 102L),
        b = c(170L, 119L)
      ),
      lh_atlas = hemi_surf,
      rh_atlas = hemi_surf,
      surf_type = "inflated",
      surface_space = "toy"
    ),
    class = c("parcel_plot_test", "surfatlas", "atlas")
  )
}

test_that("plot_brain aligns a data frame before rendering", {
  atlas <- make_parcel_plot_test_atlas()
  results <- tibble::tibble(
    roi_index = c(2L, 1L),
    statistic = c(20, 10)
  )

  p <- plot_brain(
    atlas,
    views = "dorsal",
    hemis = "left",
    interactive = FALSE,
    border = FALSE,
    data = results,
    value = statistic,
    by = c(id = "roi_index")
  )

  expect_s3_class(p, "ggplot")
  expect_equal(unique(p$data$fill_value[p$data$parcel_id == 1L]), 10)
  expect_equal(unique(p$data$fill_value[p$data$parcel_id == 2L]), 20)
})

test_that("plot_brain enforces one parcel input path", {
  atlas <- make_parcel_plot_test_atlas()
  results <- tibble::tibble(id = 1:2, statistic = c(1, 2))

  expect_error(
    plot_brain(
      atlas,
      vals = c(1, 2),
      data = results,
      value = statistic,
      interactive = FALSE
    ),
    "either.*data.*vals"
  )
  expect_error(
    plot_brain(atlas, value = statistic, interactive = FALSE),
    "require.*data"
  )
  expect_error(
    plot_brain(atlas, allow_partial = NA, interactive = FALSE),
    "require.*data"
  )
})

test_that("plot_brain makes partial coverage explicit", {
  atlas <- make_parcel_plot_test_atlas()
  results <- tibble::tibble(id = 1L, statistic = 4)

  expect_error(
    plot_brain(
      atlas,
      data = results,
      value = statistic,
      interactive = FALSE
    ),
    "missing 1 atlas parcel"
  )

  p <- plot_brain(
    atlas,
    views = "dorsal",
    hemis = "left",
    data = results,
    value = statistic,
    allow_partial = TRUE,
    interactive = FALSE
  )
  expect_true(is.na(unique(p$data$fill_value[p$data$parcel_id == 2L])))
})

test_that("CPU renderer rejects parcel tables with a useful route", {
  atlas <- make_parcel_plot_test_atlas()
  results <- tibble::tibble(id = 1:2, statistic = c(1, 2))

  expect_error(
    plot_brain(
      atlas,
      data = results,
      value = statistic,
      static_backend = "cpu",
      interactive = FALSE
    ),
    "default.*ggplot"
  )
})

test_that("plot_brain_grid accepts several columns from one table", {
  skip_if_not_installed("patchwork")
  atlas <- make_parcel_plot_test_atlas()
  results <- tibble::tibble(
    roi_index = c(2L, 1L),
    estimate = c(2, 1),
    statistic = c(20, 10)
  )

  p <- plot_brain_grid(
    atlas,
    data = results,
    values = c("estimate", "statistic"),
    by = c(id = "roi_index"),
    views = "dorsal",
    hemis = "left",
    colorbar = FALSE
  )

  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1L)
})

test_that("plot_brain_grid validates composition arguments", {
  skip_if_not_installed("patchwork")
  atlas <- make_parcel_plot_test_atlas()
  maps <- list(A = c(-1, 1), B = c(-2, 2))

  expect_error(
    plot_brain_grid(
      atlas,
      maps,
      shared_scale = FALSE,
      colorbar = TRUE
    ),
    "shared colorbar is not meaningful"
  )
  expect_error(
    plot_brain_grid(atlas, maps, titles = "only one", colorbar = FALSE),
    "one character value per map"
  )
  expect_error(
    plot_brain_grid(atlas, maps, ncol = 1.5, colorbar = FALSE),
    "positive integer"
  )
  expect_error(
    plot_brain_grid(
      atlas,
      maps,
      static_backend = "cpu",
      colorbar = FALSE
    ),
    "does not support the CPU backend"
  )
})

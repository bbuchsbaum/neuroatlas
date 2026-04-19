# Smoke tests for the rendering helpers extracted into
# R/plot_brain_render.R. These exercise the cheap-to-test code paths
# (no real surface mesh required): colorbar construction with each
# orientation, .compose_plot_brain_figure's no-colorbar shortcut, and
# its missing-patchwork error path.

# ---- .make_colorbar_panel ---------------------------------------------------

test_that(".make_colorbar_panel returns a ggplot for both positions", {
  skip_if_not_installed("scico")
  skip_if_not_installed("scales")

  p_right <- .make_colorbar_panel(
    palette = "vik", lim = c(-1, 1), title = "z",
    position = "right"
  )
  expect_s3_class(p_right, "ggplot")

  p_bottom <- .make_colorbar_panel(
    palette = "vik", lim = c(-1, 1), title = "z",
    position = "bottom"
  )
  expect_s3_class(p_bottom, "ggplot")
})

test_that(".make_colorbar_panel rejects invalid position keywords", {
  skip_if_not_installed("scico")
  expect_error(
    .make_colorbar_panel(palette = "vik", lim = c(0, 1), position = "top"),
    "should be one of"
  )
})


# ---- .compose_plot_brain_figure --------------------------------------------

test_that(".compose_plot_brain_figure returns a ggplot when colorbar='none'", {
  base <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  out <- .compose_plot_brain_figure(
    main_plot = base,
    colorbar_plot = NULL,
    colorbar_position = "none",
    title = "T",
    subtitle = "S",
    caption = "C",
    bg = "white"
  )
  expect_s3_class(out, "ggplot")
})

test_that(".compose_plot_brain_figure errors clearly without patchwork when colorbar requested", {
  skip_if(requireNamespace("patchwork", quietly = TRUE),
          "patchwork is installed")
  base <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  cb <- base
  expect_error(
    .compose_plot_brain_figure(
      main_plot = base,
      colorbar_plot = cb,
      colorbar_position = "right"
    ),
    "patchwork"
  )
})

test_that(".compose_plot_brain_figure stitches main + colorbar via patchwork when available", {
  skip_if_not_installed("patchwork")
  base <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  cb <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  out_right <- .compose_plot_brain_figure(
    main_plot = base, colorbar_plot = cb, colorbar_position = "right"
  )
  out_bottom <- .compose_plot_brain_figure(
    main_plot = base, colorbar_plot = cb, colorbar_position = "bottom"
  )
  # patchwork::wrap_plots() returns a "patchwork" object that inherits "ggplot".
  expect_s3_class(out_right, "patchwork")
  expect_s3_class(out_bottom, "patchwork")
})

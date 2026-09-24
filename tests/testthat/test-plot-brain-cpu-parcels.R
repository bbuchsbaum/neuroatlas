.parcel_sphere_geometry <- function(hemi, n_lat = 14L, n_lon = 28L, radius = 40) {
  theta <- seq(0, pi, length.out = n_lat + 2L)[-c(1L, n_lat + 2L)]
  phi <- seq(0, 2 * pi, length.out = n_lon + 1L)[-(n_lon + 1L)]
  grid <- expand.grid(phi = phi, theta = theta)
  body <- cbind(radius * sin(grid$theta) * cos(grid$phi),
                1.6 * radius * sin(grid$theta) * sin(grid$phi),
                radius * cos(grid$theta))
  vertices <- rbind(body, c(0, 0, radius), c(0, 0, -radius))
  north <- nrow(body) + 1L
  south <- nrow(body) + 2L
  idx <- function(i, j) (i - 1L) * n_lon + ((j - 1L) %% n_lon) + 1L
  faces <- list()
  for (i in seq_len(n_lat - 1L)) for (j in seq_len(n_lon)) {
    faces[[length(faces) + 1L]] <- c(idx(i, j), idx(i + 1L, j), idx(i, j + 1L))
    faces[[length(faces) + 1L]] <- c(idx(i, j + 1L), idx(i + 1L, j), idx(i + 1L, j + 1L))
  }
  for (j in seq_len(n_lon)) {
    faces[[length(faces) + 1L]] <- c(north, idx(1L, j), idx(1L, j + 1L))
    faces[[length(faces) + 1L]] <- c(south, idx(n_lat, j + 1L), idx(n_lat, j))
  }
  neurosurf::SurfaceGeometry(vertices, do.call(rbind, faces) - 1L, hemi = hemi)
}

# Two-hemisphere toy surfatlas: four parcels per hemisphere (ids 1-4 left,
# 5-8 right) in longitude sectors, plus a medial-wall cap (label 0) on the
# medial side of each hemisphere.
.make_parcel_render_atlas <- function() {
  skip_if_not_installed("neurosurf")
  hemi_surface <- function(hemi, offset) {
    geom <- .parcel_sphere_geometry(hemi)
    v <- t(geom@mesh$vb[1:3, ])
    sector <- as.integer(cut(atan2(v[, 3], v[, 2]),
                             c(-pi, -pi / 2, 0, pi / 2, pi + 1e-9),
                             include.lowest = TRUE))
    medial_side <- if (hemi == "lh") v[, 1] > 30 else v[, 1] < -30
    labels <- sector + offset
    labels[medial_side] <- 0L
    methods::new("LabeledNeuroSurface", labels = paste0("p", 1:4),
                 cols = rep("#888888", 4), geometry = geom,
                 indices = seq_len(nrow(v)), data = labels)
  }
  lh <- hemi_surface("lh", 0L)
  rh <- hemi_surface("rh", 4L)
  structure(
    list(
      name = "parcel-render-test", ids = 1:8,
      labels = paste0("p", 1:8), orig_labels = paste0("p", 1:8),
      hemi = rep(c("left", "right"), each = 4),
      cmap = data.frame(r = 1:8, g = 1:8, b = 1:8),
      lh_atlas = lh, rh_atlas = rh,
      surf_type = "white", surface_space = "toy", density = NULL
    ),
    class = c("surfatlas", "atlas")
  )
}

.render_figure_png <- function(fig, width, height) {
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = width, height = height, res = 72)
  on.exit(unlink(file))
  print(fig)
  grDevices::dev.off()
  img <- png::readPNG(file)
  list(file = file, img = img)
}

test_that("CPU parcel renderer returns a printable figure with provenance", {
  skip_if_not_installed("png")
  atl <- .make_parcel_render_atlas()
  vals <- c(3, NA, -2.5, 1, NA, 2.2, NA, -3)
  anatomy <- list(
    lh = rnorm(length(atl$lh_atlas@data)),
    rh = rnorm(length(atl$rh_atlas@data))
  )
  fig <- plot_brain(atl, vals = vals, interactive = FALSE,
                    static_backend = "cpu", palette = "vik", lim = c(-3, 3),
                    vals_threshold = 1.5, colorbar = TRUE,
                    colorbar_title = "z", title = "Toy", subtitle = "sub",
                    anatomy_metric = anatomy, render_width = 120L,
                    render_height = 80L, render_antialias = 2L)
  expect_s3_class(fig, "parcel_brain_figure")
  expect_identical(attr(fig, "plot_brain_backend"), "cpu_deferred_parcels")
  expect_identical(attr(fig, "plot_brain_anatomy")$lh$source, "explicit")
  expect_identical(attr(fig, "plot_brain_colorbar")$threshold, 1.5)
  expect_equal(attr(fig, "plot_brain_colorbar")$lim, c(-3, 3))
  expect_named(fig$panels, c("lh_lateral", "rh_lateral", "lh_medial",
                             "rh_medial"))

  out <- .render_figure_png(fig, 600, 380)
  expect_gt(stats::var(as.vector(out$img[, , 1])), 0)
  # Saturated red or blue pixels exist: the valued parcels are filled.
  r <- out$img[, , 1]
  b <- out$img[, , 3]
  expect_true(any(r - b > 0.3))
  expect_true(any(b - r > 0.3))
})

test_that("vals_threshold unfills sub-threshold parcels", {
  atl <- .make_parcel_render_atlas()
  anatomy <- list(lh = rep(0, length(atl$lh_atlas@data)),
                  rh = rep(0, length(atl$rh_atlas@data)))
  render <- function(threshold) {
    fig <- plot_brain(atl, vals = c(1, 1, 1, 1, 1, 1, 1, 1),
                      interactive = FALSE, static_backend = "cpu",
                      palette = c("#0000FF", "#FFFFFF", "#FF0000"),
                      lim = c(-1, 1), vals_threshold = threshold,
                      views = "lateral", hemis = "left",
                      anatomy_metric = anatomy, colorbar = FALSE,
                      render_width = 60L, render_height = 40L,
                      render_antialias = 1L)
    fig$panels$lh_lateral$raster
  }
  red_share <- function(raster) {
    rgb <- grDevices::col2rgb(raster[substr(raster, 8, 9) == "FF"])
    mean(rgb[1, ] > 200 & rgb[3, ] < 80)
  }
  expect_gt(red_share(render(NULL)), 0.3)
  expect_identical(red_share(render(2)), 0)
  expect_error(render(-1), "non-negative")
})

test_that("figure layout adapts to the canvas and colorbar placement", {
  dims <- list(
    lh_lateral = c(100, 160), rh_lateral = c(100, 160),
    lh_medial = c(100, 160), rh_medial = c(100, 160)
  )
  arrangements <- .parcel_figure_arrangements(c("lateral", "medial"),
                                              c("left", "right"))
  expect_length(arrangements, 3L)
  expect_identical(
    as.vector(arrangements[[3]]$keys),
    c("lh_lateral", "lh_medial", "rh_medial", "rh_lateral")
  )
  fake <- list(title = NULL, subtitle = NULL, caption = NULL)
  scale_for <- function(arr, w, h, pos = "bottom") {
    m <- .parcel_figure_metrics(fake, w, h)
    .parcel_figure_fit(arr, dims, FALSE, pos, m, w, h)$s
  }
  # A wide canvas favours the single row; a squarer one the 2x2 grid.
  expect_gt(scale_for(arrangements[[3]], 12, 3), scale_for(arrangements[[1]], 12, 3))
  expect_gt(scale_for(arrangements[[1]], 8, 6), scale_for(arrangements[[3]], 8, 6))
  grid_fit <- .parcel_figure_fit(arrangements[[1]], dims, FALSE, "right",
                                 .parcel_figure_metrics(fake, 8, 6), 8, 6)
  expect_true(grid_fit$headers)
  expect_false(.parcel_figure_fit(arrangements[[1]], dims, TRUE, "right",
                                  .parcel_figure_metrics(fake, 8, 6), 8, 6)$headers)
})

test_that("figure draws with every colorbar placement and explicit labels", {
  skip_if_not_installed("png")
  atl <- .make_parcel_render_atlas()
  anatomy <- list(lh = rep(0, length(atl$lh_atlas@data)),
                  rh = rep(0, length(atl$rh_atlas@data)))
  for (cb in list(TRUE, FALSE, "right", "bottom")) {
    fig <- plot_brain(atl, vals = c(2, NA, NA, NA, NA, NA, NA, -2),
                      interactive = FALSE, static_backend = "cpu",
                      colorbar = cb, colorbar_title = "Statistic",
                      caption = "caption", anatomy_metric = anatomy,
                      panel_labels = if (isTRUE(cb)) c("A", "B", "C", "D"),
                      render_width = 60L, render_height = 40L,
                      render_antialias = 1L)
    expect_silent(.render_figure_png(fig, 400, 260))
  }
})

test_that("colorbar ticks include the threshold and drop colliding ticks", {
  ticks <- .parcel_colorbar_ticks(c(-3.2, 3.2), 1.96)
  expect_true(all(c(-1.96, 1.96) %in% ticks$at))
  expect_false(any(c(-2, -1, 1, 2) %in% ticks$at))
  expect_true(0 %in% ticks$at)
  expect_identical(ticks$labels[ticks$at == -3.2], "\u22123.2")
  plain <- .parcel_colorbar_ticks(c(-1, 1), NULL)
  expect_identical(plain$at, c(-1, -0.5, 0, 0.5, 1))
})

test_that("parcel preparation is cached per atlas and settings", {
  atl <- .make_parcel_render_atlas()
  anatomy <- list(lh = rep(0, length(atl$lh_atlas@data)),
                  rh = rep(0, length(atl$rh_atlas@data)))
  a <- .parcel_surface_prep(atl, "lh", anatomy_metric = anatomy)
  b <- .parcel_surface_prep(atl, "lh", anatomy_metric = anatomy)
  expect_identical(a, b)
  c2 <- .parcel_surface_prep(atl, "lh", anatomy_metric = anatomy,
                             boundary_smooth = 0L)
  expect_identical(c2$boundary_smooth, 0L)
  # White display surfaces fall back to computed curvature, offline.
  d <- .parcel_surface_prep(atl, "rh")
  expect_true(nzchar(d$anatomy_source))
})

test_that("inflated fsaverage6 atlases get the sulcal-depth underlay", {
  skip_if_not_installed("neurosurf")
  env <- new.env()
  utils::data("fsaverage", package = "neuroatlas", envir = env)
  skip_if(is.null(env$fsaverage$lh_inflated), "packaged fsaverage6 meshes unavailable")
  geom <- env$fsaverage$lh_inflated
  n <- ncol(geom@mesh$vb)
  hemi_surf <- methods::new("LabeledNeuroSurface", labels = "p1", cols = "#888888",
                            geometry = geom, indices = seq_len(n),
                            data = rep(1L, n))
  # Mirrors get_schaefer_surfatlas(): a declared density must not divert the
  # white-mesh lookup away from the packaged meshes.
  atl <- list(ids = 1L, lh_atlas = hemi_surf, rh_atlas = hemi_surf,
              surf_type = "inflated", surface_space = "fsaverage6",
              density = "41k")
  class(atl) <- c("surfatlas", "atlas")
  anatomy <- .resolve_parcel_anatomy(atl, "lh")
  expect_identical(anatomy$source, "sulcal_proxy_white_vs_display")
  expect_length(anatomy$metric, n)
  expect_true(all(is.finite(anatomy$metric)))
})

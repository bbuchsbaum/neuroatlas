.plot_brain_test_atlas <- local({
  cache <- new.env(parent = emptyenv())

  function(surf = c("inflated", "white", "pial")) {
    surf <- match.arg(surf)
    key <- paste0("schaefer_100_7_", surf)

    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }

    atl <- tryCatch(
      schaefer_surf(
        parcels = 100,
        networks = 7,
        space = "fsaverage6",
        surf = surf
      ),
      error = function(e) {
        skip(paste0(
          "Schaefer surface atlas unavailable for surf='", surf,
          "': ", conditionMessage(e)
        ))
      }
    )

    assign(key, atl, envir = cache)
    atl
  }
})

.expected_surface_panels <- function(views = c("lateral", "medial",
                                                "dorsal", "ventral")) {
  as.vector(outer(c("Left", "Right"), tools::toTitleCase(views), paste))
}

.summarize_panel_extents <- function(polygons) {
  polygons |>
    dplyr::group_by(panel, hemi, view) |>
    dplyr::summarise(
      n_vertices = dplyr::n(),
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y),
      width = xmax - xmin,
      height = ymax - ymin,
      cx = (xmin + xmax) / 2,
      cy = (ymin + ymax) / 2,
      .groups = "drop"
    )
}

.left_right_view_ratios <- function(extents) {
  extents |>
    dplyr::select(hemi, view, width, height) |>
    tidyr::pivot_wider(
      names_from = hemi,
      values_from = c(width, height)
    ) |>
    dplyr::mutate(
      width_ratio = width_left / width_right,
      height_ratio = height_left / height_right
    )
}

.summarize_ggseg_panel_extents <- function(atlas_string = "schaefer7_100") {
  gg_atlas <- neuroatlas:::.load_ggseg_schaefer_atlas(atlas_string)
  dat <- gg_atlas$data
  dat <- dat[dat$side %in% c("lateral", "medial"), , drop = FALSE]

  by_panel <- split(dat, interaction(dat$hemi, dat$side, drop = TRUE))

  rows <- lapply(names(by_panel), function(key) {
    sub <- by_panel[[key]]
    bb <- sf::st_bbox(sub)
    parts <- strsplit(key, ".", fixed = TRUE)[[1]]
    hemi <- parts[1]
    view <- parts[2]

    data.frame(
      panel = paste(tools::toTitleCase(hemi), tools::toTitleCase(view)),
      hemi = hemi,
      view = view,
      xmin = as.numeric(bb[["xmin"]]),
      xmax = as.numeric(bb[["xmax"]]),
      ymin = as.numeric(bb[["ymin"]]),
      ymax = as.numeric(bb[["ymax"]]),
      stringsAsFactors = FALSE
    )
  })

  out <- dplyr::bind_rows(rows)
  out$width <- out$xmax - out$xmin
  out$height <- out$ymax - out$ymin
  out$cx <- (out$xmin + out$xmax) / 2
  out$cy <- (out$ymin + out$ymax) / 2

  tibble::as_tibble(out)
}

test_that("surface polygon data has complete, non-degenerate panel layout", {
  skip_on_cran()

  surfs <- c("inflated", "white", "pial")
  views <- c("lateral", "medial", "dorsal", "ventral")
  expected_panels <- .expected_surface_panels(views)

  for (surf in surfs) {
    atl <- .plot_brain_test_atlas(surf)

    for (merged in c(TRUE, FALSE)) {
      built <- build_surface_polygon_data(
        atl,
        views = views,
        surface = surf,
        merged = merged,
        use_cache = FALSE
      )

      poly <- built$polygons
      expect_s3_class(poly, "tbl_df")
      expect_gt(nrow(poly), 0L)

      ext <- .summarize_panel_extents(poly)

      expect_setequal(ext$panel, expected_panels)
      expect_true(all(ext$n_vertices > 100L))
      expect_true(all(is.finite(as.matrix(ext[, c("xmin", "xmax", "ymin", "ymax")]))))
      expect_true(all(ext$width > 1))
      expect_true(all(ext$height > 1))

      ratio_tbl <- .left_right_view_ratios(ext)
      expect_true(all(ratio_tbl$width_ratio > 0.8 & ratio_tbl$width_ratio < 1.25))
      expect_true(all(ratio_tbl$height_ratio > 0.8 & ratio_tbl$height_ratio < 1.25))

      # Catch "tiny panel" regressions where one view collapses relative to
      # others under shared facet scales.
      expect_gt(min(ext$width) / max(ext$width), 0.25)
      expect_gt(min(ext$height) / max(ext$height), 0.50)
    }
  }
})

test_that("merged and triangle polygon builders preserve panel extents", {
  skip_on_cran()

  atl <- .plot_brain_test_atlas("inflated")
  views <- c("lateral", "medial", "dorsal", "ventral")

  ext_merged <- build_surface_polygon_data(
    atl,
    views = views,
    surface = "inflated",
    merged = TRUE,
    use_cache = FALSE
  )$polygons |>
    .summarize_panel_extents()

  ext_triangle <- build_surface_polygon_data(
    atl,
    views = views,
    surface = "inflated",
    merged = FALSE,
    use_cache = FALSE
  )$polygons |>
    .summarize_panel_extents()

  comp <- dplyr::inner_join(
    ext_merged,
    ext_triangle,
    by = c("panel", "hemi", "view"),
    suffix = c("_merged", "_triangle")
  )

  expect_equal(nrow(comp), length(.expected_surface_panels(views)))
  expect_equal(comp$width_merged, comp$width_triangle, tolerance = 1e-6)
  expect_equal(comp$height_merged, comp$height_triangle, tolerance = 1e-6)
  expect_equal(comp$cx_merged, comp$cx_triangle, tolerance = 1e-6)
  expect_equal(comp$cy_merged, comp$cy_triangle, tolerance = 1e-6)
})

test_that("plot_brain static output composes with patchwork", {
  skip_on_cran()
  skip_if_not_installed("patchwork")

  atl <- .plot_brain_test_atlas("inflated")

  p_lm <- plot_brain(
    atl,
    views = c("lateral", "medial"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 2
  )

  p_dv <- plot_brain(
    atl,
    views = c("dorsal", "ventral"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 2
  )

  composed <- patchwork::wrap_plots(p_lm, p_dv, ncol = 1)

  expect_s3_class(composed, "patchwork")

  grob <- patchwork::patchworkGrob(composed)
  expect_s3_class(grob, "gtable")
  expect_gt(length(grob$grobs), 0L)
  expect_true(any(grepl("^panel", grob$layout$name)))
})

test_that("plot_brain hemisphere balance tracks ggseg reference", {
  skip_on_cran()
  skip_if_not_installed("ggseg")
  skip_if_not_installed("ggsegSchaefer")
  skip_if_not_installed("sf")

  atl <- .plot_brain_test_atlas("inflated")

  ours <- build_surface_polygon_data(
    atl,
    views = c("lateral", "medial"),
    surface = "inflated",
    merged = TRUE,
    use_cache = FALSE
  )$polygons |>
    .summarize_panel_extents()

  ggseg <- .summarize_ggseg_panel_extents("schaefer7_100")

  ours_ratio <- .left_right_view_ratios(ours) |>
    dplyr::select(
      view,
      width_ratio_ours = width_ratio,
      height_ratio_ours = height_ratio
    )

  ggseg_ratio <- .left_right_view_ratios(ggseg) |>
    dplyr::select(
      view,
      width_ratio_ggseg = width_ratio,
      height_ratio_ggseg = height_ratio
    )

  cmp <- dplyr::left_join(ours_ratio, ggseg_ratio, by = "view")

  expect_false(anyNA(cmp$width_ratio_ggseg))
  expect_false(anyNA(cmp$height_ratio_ggseg))

  expect_true(all(abs(cmp$width_ratio_ours - cmp$width_ratio_ggseg) < 0.10))
  expect_true(all(abs(cmp$height_ratio_ours - cmp$height_ratio_ggseg) < 0.10))
})

test_that("presentation panel layout improves cross-view footprint balance", {
  skip_on_cran()

  atl <- .plot_brain_test_atlas("inflated")
  views <- c("lateral", "medial", "dorsal", "ventral")

  p_native <- plot_brain(
    atl,
    views = views,
    surface = "inflated",
    interactive = FALSE,
    panel_layout = "native"
  )

  p_present <- plot_brain(
    atl,
    views = views,
    surface = "inflated",
    interactive = FALSE,
    panel_layout = "presentation",
    projection_smooth = 1L
  )

  ext_native <- .summarize_panel_extents(p_native$data)
  ext_present <- .summarize_panel_extents(p_present$data)

  native_ratio <- min(ext_native$width) / max(ext_native$width)
  present_ratio <- min(ext_present$width) / max(ext_present$width)

  expect_lt(native_ratio, 0.6)
  expect_gt(present_ratio, 0.9)

  # Dorsal/ventral should be rotated to a wider footprint in presentation mode.
  dv_native <- ext_native[ext_native$view %in% c("dorsal", "ventral"), , drop = FALSE]
  dv_present <- ext_present[ext_present$view %in% c("dorsal", "ventral"), , drop = FALSE]
  expect_true(all(dv_native$width < dv_native$height))
  expect_true(all(dv_present$width > dv_present$height))
})

test_that("ggseg_like style defaults to presentation layout", {
  skip_on_cran()

  atl <- .plot_brain_test_atlas("inflated")
  views <- c("lateral", "medial", "dorsal", "ventral")

  p_style <- plot_brain(
    atl,
    views = views,
    surface = "inflated",
    interactive = FALSE,
    style = "ggseg_like"
  )

  p_present <- plot_brain(
    atl,
    views = views,
    surface = "inflated",
    interactive = FALSE,
    panel_layout = "presentation",
    projection_smooth = 1L
  )

  ext_style <- .summarize_panel_extents(p_style$data)
  ext_present <- .summarize_panel_extents(p_present$data)

  cmp <- dplyr::inner_join(
    ext_style,
    ext_present,
    by = c("panel", "hemi", "view"),
    suffix = c("_style", "_present")
  )

  expect_equal(nrow(cmp), length(.expected_surface_panels(views)))
  expect_equal(cmp$width_style, cmp$width_present, tolerance = 1e-4)
  expect_equal(cmp$height_style, cmp$height_present, tolerance = 1e-4)
})

test_that("projection_smooth changes projected geometry while preserving panels", {
  skip_on_cran()

  atl <- .plot_brain_test_atlas("inflated")
  views <- c("lateral", "medial", "dorsal", "ventral")

  built0 <- build_surface_polygon_data(
    atl,
    views = views,
    surface = "inflated",
    merged = TRUE,
    projection_smooth = 0L,
    use_cache = FALSE
  )

  built1 <- build_surface_polygon_data(
    atl,
    views = views,
    surface = "inflated",
    merged = TRUE,
    projection_smooth = 1L,
    use_cache = FALSE
  )

  expect_setequal(unique(built0$polygons$panel), unique(built1$polygons$panel))
  expect_equal(nrow(built0$polygons), nrow(built1$polygons))
  expect_true(any(abs(built1$polygons$x - built0$polygons$x) > 1e-8))
  expect_true(any(abs(built1$polygons$y - built0$polygons$y) > 1e-8))

  ext1 <- .summarize_panel_extents(built1$polygons)
  expect_true(all(is.finite(ext1$width)))
  expect_true(all(is.finite(ext1$height)))
  expect_true(all(ext1$width > 0))
  expect_true(all(ext1$height > 0))
})

#!/usr/bin/env Rscript

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package '", pkg, "' is not installed.", call. = FALSE)
  }
}

load_local_package <- function() {
  env_override <- Sys.getenv("NEUROATLAS_LOAD_LOCAL", "")
  use_local <- if (nzchar(env_override)) {
    tolower(env_override) %in% c("1", "true", "yes")
  } else {
    file.exists("DESCRIPTION")
  }

  has_installed <- requireNamespace("neuroatlas", quietly = TRUE)

  if (use_local || (!has_installed && file.exists("DESCRIPTION"))) {
    ensure_pkg("devtools")
    devtools::load_all(".")
  } else {
    if (!has_installed) {
      stop(
        "Package 'neuroatlas' is not installed. Set NEUROATLAS_LOAD_LOCAL=1 ",
        "to force loading from this repository.",
        call. = FALSE
      )
    }
  }
}

summarize_panel_extents <- function(polygons) {
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

left_right_ratios <- function(extents) {
  extents |>
    dplyr::select(hemi, view, width, height) |>
    tidyr::pivot_wider(names_from = hemi, values_from = c(width, height)) |>
    dplyr::mutate(
      width_ratio = width_left / width_right,
      height_ratio = height_left / height_right
    )
}

summarize_ggseg_extents <- function(atlas_string) {
  gg_atlas <- neuroatlas:::.load_ggseg_schaefer_atlas(atlas_string)
  dat <- gg_atlas$data
  dat <- dat[dat$side %in% c("lateral", "medial"), , drop = FALSE]

  by_panel <- split(dat, interaction(dat$hemi, dat$side, drop = TRUE))
  rows <- lapply(names(by_panel), function(key) {
    sub <- by_panel[[key]]
    bb <- sf::st_bbox(sub)
    parts <- strsplit(key, ".", fixed = TRUE)[[1]]

    tibble::tibble(
      panel = paste(tools::toTitleCase(parts[1]), tools::toTitleCase(parts[2])),
      hemi = parts[1],
      view = parts[2],
      xmin = as.numeric(bb[["xmin"]]),
      xmax = as.numeric(bb[["xmax"]]),
      ymin = as.numeric(bb[["ymin"]]),
      ymax = as.numeric(bb[["ymax"]])
    )
  })

  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      width = xmax - xmin,
      height = ymax - ymin,
      cx = (xmin + xmax) / 2,
      cy = (ymin + ymax) / 2
    )
}

build_ggseg_reference <- function(surf_atlas, vals, atlas_string) {
  gg_atlas <- neuroatlas:::.load_ggseg_schaefer_atlas(atlas_string)

  value_tbl <- tibble::tibble(
    hemi = as.character(surf_atlas$hemi),
    key = sub("^[LR]H_", "", surf_atlas$orig_labels),
    value = as.numeric(vals)
  )

  gg_atlas$data <- gg_atlas$data |>
    dplyr::mutate(
      hemi = as.character(hemi),
      key = sub("^[lr]h_17Networks_[LR]H_", "", label)
    ) |>
    dplyr::left_join(value_tbl, by = c("hemi", "key"))

  p <- ggseg::ggseg(
    atlas = gg_atlas,
    position = "stacked",
    colour = "gray70",
    mapping = ggplot2::aes(fill = value)
  ) +
    ggplot2::scale_fill_gradient2(
      low = "#3B4CC0",
      mid = "#F7F7F7",
      high = "#B40426",
      midpoint = 0,
      na.value = "grey55"
    ) +
    ggplot2::ggtitle("ggseg reference (lateral/medial)")

  list(plot = p, atlas = gg_atlas)
}

build_overlay_signal <- function(labeled_surface, x_shift = 0) {
  verts <- t(labeled_surface@geometry@mesh$vb[1:3, , drop = FALSE])
  x <- verts[, 1]
  y <- verts[, 2]
  z <- verts[, 3]

  bump_1 <- exp(-((x - 20 - x_shift)^2 + (y + 14)^2 + (z - 8)^2) / (2 * 20^2))
  bump_2 <- -0.9 * exp(-((x + 24 - x_shift)^2 + (y - 8)^2 + (z + 10)^2) /
    (2 * 18^2))
  bump_3 <- 0.55 * exp(-((x)^2 + (y)^2 + (z + 28)^2) / (2 * 24^2))

  bump_1 + bump_2 + bump_3
}

build_parcel_values <- function(atl) {
  roi <- tibble::tibble(
    id = atl$ids,
    hemi = atl$hemi,
    network = atl$network
  )

  net_index <- as.numeric(factor(roi$network))
  hemi_effect <- ifelse(roi$hemi == "left", 1, -1)
  signal <- scale(net_index) * 0.75 +
    hemi_effect * 0.35 +
    sin(roi$id / 11) * 0.30 +
    cos(roi$id / 37) * 0.20

  as.numeric(signal)
}

main <- function() {
  needed <- c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "tibble",
    "jsonlite",
    "patchwork"
  )
  invisible(lapply(needed, ensure_pkg))

  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1) args[[1]] else {
    file.path("tmp", "plot_brain_complex_example")
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  load_local_package()

  set.seed(20260221)

  message("Loading Schaefer 400x17 surface atlas (fsaverage6, inflated)...")
  surf_atlas <- neuroatlas::schaefer_surf(
    parcels = 400,
    networks = 17,
    space = "fsaverage6",
    surf = "inflated"
  )

  vals <- build_parcel_values(surf_atlas)

  overlay <- list(
    lh = build_overlay_signal(surf_atlas$lh_atlas, x_shift = -5),
    rh = build_overlay_signal(surf_atlas$rh_atlas, x_shift = 5)
  )

  message("Rendering plot_brain figures...")
  p_custom <- neuroatlas::plot_brain(
    surf_atlas,
    vals = vals,
    views = c("lateral", "medial", "dorsal", "ventral"),
    hemis = c("left", "right"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 4,
    palette = "vik",
    border = TRUE,
    border_color = "white",
    border_size = 0.12,
    silhouette = TRUE,
    silhouette_color = "black",
    silhouette_size = 0.25,
    network_border = TRUE,
    network_border_color = "black",
    network_border_size = 0.35,
    shading = TRUE,
    shading_strength = 0.18,
    fill_alpha = 0.98,
    bg = "#f4f4f2"
  ) +
    ggplot2::ggtitle("plot_brain: Schaefer 400x17 (all views)")

  p_ggseg_like <- neuroatlas::plot_brain(
    surf_atlas,
    vals = vals,
    views = c("lateral", "medial", "dorsal", "ventral"),
    hemis = c("left", "right"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 4,
    palette = "vik",
    style = "ggseg_like",
    boundary_smooth = 2L,
    bg = "#f4f4f2"
  ) +
    ggplot2::ggtitle("plot_brain: ggseg_like (boundary_smooth = 2)")

  p_ggseg_like_strong <- neuroatlas::plot_brain(
    surf_atlas,
    vals = vals,
    views = c("lateral", "medial", "dorsal", "ventral"),
    hemis = c("left", "right"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 4,
    palette = "vik",
    style = "ggseg_like",
    boundary_smooth = 3L,
    projection_smooth = 2L,
    bg = "#f4f4f2"
  ) +
    ggplot2::ggtitle(
      "plot_brain: ggseg_like (boundary_smooth = 3, projection_smooth = 2)"
    )

  p_overlay <- neuroatlas::plot_brain(
    surf_atlas,
    vals = vals,
    views = c("lateral", "medial", "dorsal", "ventral"),
    hemis = c("left", "right"),
    surface = "inflated",
    interactive = FALSE,
    ncol = 4,
    palette = "vik",
    border = TRUE,
    border_color = "white",
    border_size = 0.10,
    silhouette = TRUE,
    silhouette_color = "grey20",
    silhouette_size = 0.20,
    shading = TRUE,
    shading_strength = 0.15,
    fill_alpha = 0.92,
    overlay = overlay,
    overlay_threshold = 0.08,
    overlay_alpha = 0.55,
    overlay_palette = "broc",
    overlay_border = TRUE,
    overlay_border_color = "black",
    overlay_border_size = 0.20,
    bg = "#f4f4f2"
  ) +
    ggplot2::ggtitle("plot_brain + synthetic vertex overlay")

  p_compare <- p_custom / p_overlay + patchwork::plot_layout(heights = c(1, 1))

  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_brain_complex.png"),
    plot = p_custom,
    width = 16,
    height = 8,
    dpi = 180,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_brain_complex_overlay.png"),
    plot = p_overlay,
    width = 16,
    height = 8,
    dpi = 180,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_brain_ggseg_like.png"),
    plot = p_ggseg_like,
    width = 16,
    height = 8,
    dpi = 220,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_brain_ggseg_like_strong.png"),
    plot = p_ggseg_like_strong,
    width = 16,
    height = 8,
    dpi = 260,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_brain_complex_stack.png"),
    plot = p_compare,
    width = 16,
    height = 14,
    dpi = 180,
    bg = "white"
  )

  message("Computing layout metrics...")
  panel_extents <- neuroatlas::build_surface_polygon_data(
    surf_atlas,
    views = c("lateral", "medial", "dorsal", "ventral"),
    surface = "inflated",
    merged = TRUE,
    use_cache = FALSE
  )$polygons |>
    summarize_panel_extents()

  ratio_tbl <- left_right_ratios(panel_extents)

  write.csv(
    panel_extents,
    file = file.path(out_dir, "plot_brain_panel_extents.csv"),
    row.names = FALSE
  )

  write.csv(
    ratio_tbl,
    file = file.path(out_dir, "plot_brain_left_right_ratios.csv"),
    row.names = FALSE
  )

  has_ggseg <- requireNamespace("ggseg", quietly = TRUE) &&
    requireNamespace("ggsegSchaefer", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)

  summary_obj <- list(
    plot_brain_ratio = ratio_tbl,
    ggseg_ratio = NULL,
    ratio_delta = NULL
  )

  if (has_ggseg) {
    message("Rendering ggseg reference figure...")
    ggseg_ref <- build_ggseg_reference(
      surf_atlas = surf_atlas,
      vals = vals,
      atlas_string = "schaefer17_400"
    )
    p_ggseg <- ggseg_ref$plot

    p_side_by_side <- p_custom / p_ggseg + patchwork::plot_layout(heights = c(2, 1))

    ggplot2::ggsave(
      filename = file.path(out_dir, "plot_brain_vs_ggseg.png"),
      plot = p_side_by_side,
      width = 16,
      height = 14,
      dpi = 180,
      bg = "white"
    )

    ggseg_extents <- summarize_ggseg_extents("schaefer17_400")
    ggseg_ratio <- left_right_ratios(ggseg_extents)

    ratio_delta <- dplyr::inner_join(
      ratio_tbl |>
        dplyr::select(
          view,
          width_ratio_plot_brain = width_ratio,
          height_ratio_plot_brain = height_ratio
        ),
      ggseg_ratio |>
        dplyr::select(
          view,
          width_ratio_ggseg = width_ratio,
          height_ratio_ggseg = height_ratio
        ),
      by = "view"
    ) |>
      dplyr::mutate(
        width_ratio_abs_diff = abs(width_ratio_plot_brain - width_ratio_ggseg),
        height_ratio_abs_diff = abs(height_ratio_plot_brain - height_ratio_ggseg)
      )

    write.csv(
      ratio_delta,
      file = file.path(out_dir, "plot_brain_vs_ggseg_ratio_delta.csv"),
      row.names = FALSE
    )

    summary_obj$ggseg_ratio <- ggseg_ratio
    summary_obj$ratio_delta <- ratio_delta
  } else {
    message("ggseg/ggsegSchaefer/sf not available; skipping ggseg reference.")
  }

  jsonlite::write_json(
    summary_obj,
    path = file.path(out_dir, "plot_brain_complex_summary.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows"
  )

  message("Done. Outputs written to: ", normalizePath(out_dir))
}

main()

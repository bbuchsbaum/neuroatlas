.surface_anatomy_cache <- new.env(parent = emptyenv())

.surface_hemi_value <- function(x, hemi) {
  if (is.null(x)) return(NULL)
  if (!is.list(x)) return(x)
  x[[hemi]] %||% x[[if (hemi == "lh") "left" else "right"]]
}

.resolve_surface_domain <- function(surfatlas, hemi, override = NULL,
                                    source = NULL) {
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  n <- length(atlas_hemi@data)
  mask <- .surface_hemi_value(override, hemi) %||%
    .surface_hemi_value(surfatlas$cortex_mask, hemi)
  if (is.null(mask)) {
    mask <- rep(TRUE, n)
    source <- source %||% "geometry_all_vertices"
  } else {
    source <- source %||% surfatlas$cortex_mask_source %||% "explicit"
  }
  if (!is.logical(mask) || length(mask) != n || anyNA(mask)) {
    stop("Cortex mask for ", hemi, " must be one non-missing logical value per vertex.",
         call. = FALSE)
  }
  list(mask = mask, provenance = list(
    source = source,
    surface_space = surfatlas$surface_space %||% NA_character_,
    density = surfatlas$density %||% NA_character_,
    hemi = hemi,
    surface_type = surfatlas$surf_type %||% NA_character_,
    n_cortex = sum(mask),
    n_medial_wall = sum(!mask)
  ))
}

.compute_surface_curvature_cached <- function(geometry, cache_key) {
  if (exists(cache_key, envir = .surface_anatomy_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .surface_anatomy_cache, inherits = FALSE))
  }
  metric <- tryCatch(neurosurf::curvature(geometry), error = function(e) NULL)
  assign(cache_key, metric, envir = .surface_anatomy_cache)
  metric
}

.surface_geometry_topology_equal <- function(x, y) {
  if (!inherits(x, "SurfaceGeometry") || !inherits(y, "SurfaceGeometry")) {
    return(FALSE)
  }

  x_vertices <- x@mesh$vb
  y_vertices <- y@mesh$vb
  x_faces <- x@mesh$it
  y_faces <- y@mesh$it

  identical(dim(x_vertices), dim(y_vertices)) &&
    identical(dim(x_faces), dim(y_faces)) &&
    identical(unname(x_faces), unname(y_faces))
}

.resolve_surface_anatomy <- function(surfatlas, hemi, override = NULL,
                                     source = NULL) {
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  display_geometry <- atlas_hemi@geometry
  n <- length(atlas_hemi@data)
  metric <- .surface_hemi_value(override, hemi) %||%
    .surface_hemi_value(surfatlas$anatomy_metric, hemi)
  if (!is.null(metric)) {
    metric_source <- source %||% surfatlas$anatomy_metric_source %||%
      "explicit_sulcal_or_curvature_metric"
    source_surface <- surfatlas$anatomy_metric_surface %||% "declared"
    topology_verified <- FALSE
  } else {
    pair <- .resolve_overlay_surface_pair(surfatlas, hemi = hemi)
    source_geometry <- pair$white
    mesh_identity <- rlang::hash(list(
      source_geometry@mesh$vb[1:3, , drop = FALSE],
      source_geometry@mesh$it
    ))
    cache_key <- rlang::hash(list(
      surfatlas$surface_space %||% NA_character_,
      surfatlas$density %||% NA_character_,
      hemi,
      "white",
      "mean_curvature",
      mesh_identity
    ))
    metric <- .compute_surface_curvature_cached(source_geometry, cache_key)
    metric_source <- "computed_mean_curvature"
    source_surface <- "white"
    topology_verified <- .surface_geometry_topology_equal(
      source_geometry,
      display_geometry
    )
  }
  if (is.null(metric) || !is.numeric(metric) || length(metric) != n) {
    metric <- rep(0, n)
    metric_source <- "neutral_fallback"
    source_surface <- NA_character_
    topology_verified <- FALSE
  }
  list(metric = as.numeric(metric), provenance = list(
    source = metric_source,
    source_surface = source_surface,
    display_surface = surfatlas$surf_type %||% NA_character_,
    topology_verified = topology_verified && length(metric) == n,
    surface_space = surfatlas$surface_space %||% NA_character_,
    density = surfatlas$density %||% NA_character_,
    hemi = hemi,
    mesh_identity = rlang::hash(list(
      display_geometry@mesh$vb[1:3, , drop = FALSE],
      display_geometry@mesh$it
    ))
  ))
}

.surface_rgba_raster <- function(x) {
  grDevices::as.raster(array(as.numeric(x$rgba) / 255, dim = dim(x$rgba)))
}

.surface_orientation_annotations <- function(hemi, view) {
  if (view %in% c("lateral", "medial")) {
    anterior_x <- if (hemi == "left") 0.03 else 0.97
    posterior_x <- 1 - anterior_x
    return(data.frame(
      x = c(anterior_x, posterior_x), y = c(0.04, 0.04),
      label = c("A", "P")
    ))
  }
  data.frame(x = c(0.03, 0.03), y = c(0.96, 0.04), label = c("A", "P"))
}

.plot_brain_cpu <- function(surfatlas,
                            overlay,
                            views,
                            hemis,
                            overlay_threshold,
                            overlay_alpha,
                            overlay_alpha_ramp,
                            overlay_palette,
                            overlay_lim,
                            overlay_fun,
                            overlay_sampling,
                            overlay_interpolation,
                            overlay_aggregate,
                            overlay_n_samples,
                            overlay_depth,
                            overlay_surface_smooth_fwhm,
                            colorbar_position,
                            colorbar_source,
                            overlay_title,
                            title,
                            subtitle,
                            caption,
                            panel_labels,
                            bg,
                            cortex_mask,
                            cortex_mask_source,
                            anatomy_metric,
                            anatomy_metric_source,
                            medial_wall,
                            camera,
                            orientation_labels,
                            render_width,
                            render_height,
                            render_antialias) {
  if (is.null(overlay)) {
    stop("static_backend = 'cpu' currently requires a continuous overlay.",
         call. = FALSE)
  }
  projection_meta <- NULL
  if (inherits(overlay, "NeuroVol")) {
    projected <- .project_cluster_overlay(
      cluster_vol = overlay,
      surfatlas = surfatlas,
      fun = overlay_fun,
      sampling = overlay_sampling,
      interpolation = overlay_interpolation,
      aggregate = overlay_aggregate,
      n_samples = overlay_n_samples,
      depth = overlay_depth,
      surface_smooth_fwhm = overlay_surface_smooth_fwhm
    )
    overlay <- projected$overlay
    projection_meta <- projected$meta
  }
  if (!is.list(overlay)) {
    stop("CPU overlay must be a NeuroVol or lh/rh vertex-value list.",
         call. = FALSE)
  }
  finite <- unlist(overlay, use.names = FALSE)
  finite <- finite[is.finite(finite)]
  if (is.null(overlay_lim)) {
    overlay_lim <- if (length(finite)) range(finite) else c(-1, 1)
    if (overlay_lim[1] == overlay_lim[2]) {
      overlay_lim <- overlay_lim + c(-1, 1) * 1e-8
    }
  }
  alpha_ramp <- overlay_alpha_ramp %||% max(
    0.25 * abs(overlay_threshold %||% 0),
    0.06 * max(abs(overlay_lim))
  )
  palette <- scico::scico(256, palette = overlay_palette)
  panels <- list()
  provenance <- list(mask = list(), anatomy = list(), camera = list(),
                     projection = projection_meta)
  panel_levels <- character()

  for (view in views) for (hemi in hemis) {
    hk <- if (hemi == "left") "lh" else "rh"
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]
    values <- overlay[[hk]] %||% overlay[[hemi]]
    if (is.null(values) || length(values) != length(atlas_hemi@data)) {
      stop("Overlay values for ", hk, " must match its surface vertices.",
           call. = FALSE)
    }
    domain <- .resolve_surface_domain(
      surfatlas, hk, override = cortex_mask, source = cortex_mask_source
    )
    anatomy <- .resolve_surface_anatomy(
      surfatlas, hk, override = anatomy_metric, source = anatomy_metric_source
    )
    rendered <- neurosurf::render_surface_rgba(
      geometry = atlas_hemi@geometry,
      vertex_values = values,
      anatomy_metric = anatomy$metric,
      cortex_mask = domain$mask,
      camera = view,
      camera_mode = camera,
      width = render_width,
      height = render_height,
      threshold = overlay_threshold %||% 0,
      palette = palette,
      limits = overlay_lim,
      overlay_alpha = overlay_alpha,
      alpha_ramp = alpha_ramp,
      antialias = render_antialias,
      medial_wall = medial_wall,
      background = bg,
      outer_contour = TRUE
    )
    default_label <- paste0(tools::toTitleCase(hemi), " ",
                            tools::toTitleCase(view))
    panel_levels <- c(panel_levels, default_label)
    display_label <- .resolve_plot_brain_panel_labels(
      default_label, panel_labels = panel_labels
    )
    p <- ggplot2::ggplot() +
      ggplot2::annotation_raster(.surface_rgba_raster(rendered),
                                 xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      ggplot2::coord_fixed(
        ratio = render_height / render_width,
        xlim = c(0, 1), ylim = c(0, 1), expand = FALSE
      ) +
      ggplot2::theme_void() +
      ggplot2::ggtitle(display_label) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 10,
                                           face = "bold"),
        plot.background = ggplot2::element_rect(fill = bg, colour = NA)
      )
    if (isTRUE(orientation_labels)) {
      orient <- .surface_orientation_annotations(hemi, view)
      p <- p + ggplot2::geom_text(
        data = orient,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE, size = 2.5, colour = "grey30",
        fontface = "bold"
      )
    }
    panels[[length(panels) + 1L]] <- p
    provenance$mask[[hk]] <- domain$provenance
    provenance$anatomy[[hk]] <- anatomy$provenance
    provenance$camera[[default_label]] <- rendered$camera
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The 'patchwork' package is required for CPU static composition.",
         call. = FALSE)
  }
  main <- patchwork::wrap_plots(panels, ncol = min(2L, length(panels)))
  resolved_source <- .resolve_colorbar_source(
    colorbar_source, overlay_values = finite, overlay_present = TRUE
  )
  cb <- NULL
  colorbar_meta <- list(source = resolved_source)
  if (!identical(colorbar_position, "none") &&
      identical(resolved_source, "overlay")) {
    breaks <- .colorbar_break_values(overlay_lim, overlay_threshold)
    cb <- .make_colorbar_panel(
      palette = overlay_palette, lim = overlay_lim, title = overlay_title,
      position = colorbar_position, bg = bg, breaks = breaks
    )
    colorbar_meta <- c(colorbar_meta, list(
      palette = overlay_palette, lim = overlay_lim, title = overlay_title,
      breaks = breaks
    ))
  }
  out <- .compose_plot_brain_figure(
    main_plot = main, colorbar_plot = cb,
    colorbar_position = colorbar_position,
    title = title, subtitle = subtitle, caption = caption, bg = bg
  )
  attr(out, "plot_brain_colorbar") <- colorbar_meta
  attr(out, "plot_brain_projection") <- list(
    interpolation = overlay_interpolation, sampling = overlay_sampling,
    aggregate = overlay_aggregate, n_samples = overlay_n_samples,
    depth = overlay_depth,
    surface_smooth_fwhm = overlay_surface_smooth_fwhm
  )
  attr(out, "plot_brain_anatomy") <- provenance
  attr(out, "plot_brain_backend") <- "cpu_barycentric"
  out
}

.build_cluster_overlay_volume <- function(stat_map,
                                          cluster_voxels,
                                          selected_cluster_ids = NULL) {
  stat_arr <- as.array(stat_map)
  out_arr <- array(0, dim = dim(stat_arr))

  ids <- names(cluster_voxels)
  if (!is.null(selected_cluster_ids) && length(selected_cluster_ids) > 0) {
    ids <- intersect(ids, as.character(selected_cluster_ids))
  }

  if (length(ids) == 0) {
    return(neuroim2::NeuroVol(out_arr, space = neuroim2::space(stat_map)))
  }

  for (cid in ids) {
    vox <- cluster_voxels[[cid]]
    if (is.null(vox) || nrow(vox) == 0) next
    idx <- .grid_to_linear_index(vox, dim(out_arr))
    out_arr[idx] <- stat_arr[idx]
  }

  neuroim2::NeuroVol(out_arr, space = neuroim2::space(stat_map))
}

.project_cluster_overlay <- function(cluster_vol,
                                     surfatlas,
                                     space_override = NULL,
                                     density_override = NULL,
                                     resolution_override = NULL,
                                     fun = c("avg", "nn", "mode"),
                                     sampling = c("midpoint",
                                                  "normal_line",
                                                  "thickness")) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)

  out <- list(lh = NULL, rh = NULL)
  meta <- list(surface_space = NULL, hemis = list())

  for (hemi in c("lh", "rh")) {
    atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
    if (is.null(atlas_hemi)) next

    pair <- .resolve_overlay_surface_pair(
      surfatlas = surfatlas,
      hemi = hemi,
      space_override = space_override,
      density_override = density_override,
      resolution_override = resolution_override
    )
    meta$surface_space <- pair$surface_space

    target_n <- length(atlas_hemi@data)
    vals <- .project_overlay_one_hemi(
      cluster_vol = cluster_vol,
      surf_wm = pair$white,
      surf_pial = pair$pial,
      target_n = target_n,
      fun = fun,
      sampling = sampling
    )

    out[[hemi]] <- vals
    meta$hemis[[hemi]] <- list(
      target_vertices = target_n,
      projected_vertices = length(vals),
      finite_vertices = sum(is.finite(vals))
    )
  }

  list(overlay = out, meta = meta)
}

.overlay_projection_diagnostics <- function(cluster_vol,
                                            projection,
                                            threshold,
                                            sampling,
                                            fun) {
  vals <- projection$overlay
  meta <- projection$meta
  cluster_vals <- as.array(cluster_vol)
  nonzero <- sum(cluster_vals != 0, na.rm = TRUE)

  hemi_stats <- lapply(c("lh", "rh"), function(h) {
    x <- vals[[h]]
    if (is.null(x)) {
      data.frame(
        hemi = h,
        target_vertices = NA_integer_,
        finite_vertices = 0L,
        above_threshold = 0L,
        finite_min = NA_real_,
        finite_max = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      finite <- is.finite(x)
      data.frame(
        hemi = h,
        target_vertices = if (!is.null(meta$hemis[[h]]$target_vertices)) {
          as.integer(meta$hemis[[h]]$target_vertices)
        } else {
          as.integer(length(x))
        },
        finite_vertices = as.integer(sum(finite)),
        above_threshold = as.integer(sum(finite & abs(x) >= threshold)),
        finite_min = if (any(finite)) min(x[finite]) else NA_real_,
        finite_max = if (any(finite)) max(x[finite]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  })
  hemi_tbl <- do.call(rbind, hemi_stats)

  list(
    cluster_voxels_nonzero = nonzero,
    surface_space = meta$surface_space,
    projection_fun = fun,
    projection_sampling = sampling,
    overlay_threshold = threshold,
    hemi = hemi_tbl
  )
}

.project_overlay_one_hemi <- function(cluster_vol,
                                      surf_wm,
                                      surf_pial,
                                      target_n,
                                      fun,
                                      sampling) {
  proj <- tryCatch(
    neurosurf::vol_to_surf(
      surf_wm = surf_wm,
      surf_pial = surf_pial,
      vol = cluster_vol,
      fun = fun,
      sampling = sampling,
      fill = 0
    ),
    error = function(e) NULL
  )

  vals <- .surface_values_to_numeric(proj)
  if (is.null(vals)) {
    return(rep(NA_real_, target_n))
  }

  if (length(vals) != target_n) {
    return(rep(NA_real_, target_n))
  }

  vals
}

.surface_values_to_numeric <- function(x) {
  if (is.null(x)) return(NULL)

  vals <- tryCatch(neurosurf::values(x), error = function(e) NULL)
  if (is.null(vals)) {
    vals <- tryCatch(x@data, error = function(e) NULL)
  }
  if (is.null(vals)) return(NULL)
  as.numeric(vals)
}

.resolve_overlay_surface_pair <- function(surfatlas,
                                          hemi = c("lh", "rh"),
                                          space_override = NULL,
                                          density_override = NULL,
                                          resolution_override = NULL) {
  hemi <- match.arg(hemi)
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  current_geom <- atlas_hemi@geometry
  surf_type <- if (!is.null(surfatlas$surf_type)) surfatlas$surf_type else NA_character_

  white <- if (identical(surf_type, "white")) current_geom else NULL
  pial <- if (identical(surf_type, "pial")) current_geom else NULL

  surface_space <- if (!is.null(space_override)) {
    space_override
  } else if (!is.null(surfatlas$surface_space)) {
    surfatlas$surface_space
  } else {
    "fsaverage6"
  }

  if (is.null(white)) {
    white <- .load_overlay_surface_geometry(
      surface_space = surface_space,
      surface_type = "white",
      hemi = hemi,
      density_override = density_override,
      resolution_override = resolution_override
    )
  }
  if (is.null(pial)) {
    pial <- .load_overlay_surface_geometry(
      surface_space = surface_space,
      surface_type = "pial",
      hemi = hemi,
      density_override = density_override,
      resolution_override = resolution_override
    )
  }

  if (is.null(white)) white <- current_geom
  if (is.null(pial)) pial <- current_geom

  list(white = white, pial = pial, surface_space = surface_space)
}

.surface_template_defaults <- function(surface_space) {
  if (is.null(surface_space) || !nzchar(surface_space)) {
    return(list(template_id = "fsaverage", density = "41k", resolution = "06"))
  }

  switch(
    as.character(surface_space),
    fsaverage6 = list(template_id = "fsaverage", density = "41k",
                      resolution = "06"),
    fsaverage5 = list(template_id = "fsaverage", density = "10k",
                      resolution = "05"),
    fsaverage = list(template_id = "fsaverage", density = "164k",
                     resolution = NULL),
    list(template_id = surface_space, density = NULL, resolution = NULL)
  )
}

.load_overlay_surface_geometry <- function(surface_space,
                                           surface_type = c("white", "pial"),
                                           hemi = c("lh", "rh"),
                                           density_override = NULL,
                                           resolution_override = NULL) {
  surface_type <- match.arg(surface_type)
  hemi <- match.arg(hemi)

  # Fast packaged fallback for fsaverage6 surfaces.
  if (identical(surface_space, "fsaverage6") &&
      is.null(density_override) &&
      is.null(resolution_override)) {
    fsaverage <- NULL
    utils::data("fsaverage", package = "neuroatlas", envir = environment())
    if (exists("fsaverage", envir = environment(), inherits = FALSE)) {
      fsaverage <- get("fsaverage", envir = environment())
      geom_name <- paste0(hemi, "_", surface_type)
      if (!is.null(fsaverage[[geom_name]])) {
        return(fsaverage[[geom_name]])
      }
    }
  }

  defaults <- .surface_template_defaults(surface_space)
  density <- if (!is.null(density_override)) density_override else defaults$density
  resolution <- if (!is.null(resolution_override)) {
    resolution_override
  } else {
    defaults$resolution
  }

  hemi_tf <- if (identical(hemi, "lh")) "L" else "R"
  tryCatch(
    load_surface_template(
      template_id = defaults$template_id,
      surface_type = surface_type,
      hemi = hemi_tf,
      density = density,
      resolution = resolution
    ),
    error = function(e) NULL
  )
}

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

#' Project a Volume onto a Surface Atlas
#'
#' @description
#' Samples a volumetric map (for example a thresholded statistic or cluster
#' map) onto the vertices of both hemispheres of a surface atlas, using the
#' same volume-to-surface projection that \code{\link{plot_brain}} applies
#' when its \code{overlay} argument is a \code{NeuroVol}. The result holds
#' one value per atlas vertex, so it can be passed back to
#' \code{plot_brain(overlay = )}, summarised, or used to build a colour key
#' that matches the rendered surface exactly.
#'
#' @param cluster_vol A \code{\link[neuroim2]{NeuroVol}} in the world (mm)
#'   space of the surface meshes, typically MNI152 for fsaverage surfaces.
#'   Zero voxels are treated as data, so mask the volume first if zero should
#'   mean "no signal".
#' @param surfatlas A surface atlas (class \code{"surfatlas"}), for example
#'   from \code{\link{schaefer_surf}}. Its vertex count per hemisphere sets
#'   the length of the returned vectors.
#' @param space_override,density_override,resolution_override Optional
#'   surface space (e.g. \code{"fsaverage6"}), TemplateFlow density, and
#'   resolution used to look up the white and pial meshes. By default these
#'   come from the atlas (\code{surfatlas$surface_space}, falling back to
#'   \code{"fsaverage6"}).
#' @param fun Vertex summary passed to \code{neurosurf::vol_to_surf()}: one
#'   of \code{"avg"}, \code{"nn"}, or \code{"mode"}.
#' @param sampling Sampling strategy between the white and pial surfaces:
#'   \code{"midpoint"}, \code{"normal_line"}, or \code{"thickness"}.
#' @param interpolation Voxel interpolation: \code{"legacy"},
#'   \code{"nearest"}, or \code{"linear"}.
#' @param aggregate Optional aggregation across depth samples
#'   (\code{"mean"}, \code{"mode"}, or \code{"closest"}).
#' @param n_samples Optional number of sampling depths.
#' @param depth Optional explicit thickness fractions or normal-line offsets.
#' @param surface_smooth_fwhm Tangential surface smoothing in mm (default
#'   \code{0}, no smoothing).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{overlay}{A list with numeric vectors \code{lh} and \code{rh},
#'     one value per vertex of the corresponding atlas hemisphere. Vertices
#'     the projection does not reach are \code{NA}. A hemisphere missing
#'     from the atlas is \code{NULL}.}
#'   \item{meta}{Projection provenance: \code{surface_space} and, per
#'     hemisphere, the vertex counts and the sampling settings that were
#'     applied (the same record \code{plot_brain()} stores).}
#' }
#'
#' @details
#' White and pial meshes are taken from the atlas when it was built on them
#' and otherwise loaded for the atlas's surface space, so the projection is
#' anatomically correct even for atlases displayed on inflated surfaces.
#'
#' If projection fails for a hemisphere (for example because the meshes
#' cannot be loaded or do not match the atlas), that hemisphere is returned
#' as all \code{NA} and a warning reports the reason; \code{plot_brain()}
#' keeps its existing silent behaviour.
#'
#' @examples
#' \dontrun{
#' atlas <- schaefer_surf(200, 7, space = "fsaverage6", surf = "inflated")
#' stat <- neuroim2::read_vol("zstat1.nii.gz")
#' proj <- project_cluster_overlay(stat, atlas, sampling = "thickness",
#'                                 interpolation = "linear")
#' range(proj$overlay$lh, na.rm = TRUE)
#'
#' # Draw exactly the projected values
#' plot_brain(atlas, overlay = proj$overlay, overlay_threshold = 3.1)
#' }
#'
#' @seealso \code{\link{plot_brain}}, \code{neurosurf::vol_to_surf()}
#' @export
project_cluster_overlay <- function(cluster_vol,
                                    surfatlas,
                                    space_override = NULL,
                                    density_override = NULL,
                                    resolution_override = NULL,
                                    fun = c("avg", "nn", "mode"),
                                    sampling = c("midpoint", "normal_line",
                                                 "thickness"),
                                    interpolation = c("legacy", "nearest",
                                                      "linear"),
                                    aggregate = NULL,
                                    n_samples = NULL,
                                    depth = NULL,
                                    surface_smooth_fwhm = 0) {
  if (!methods::is(cluster_vol, "NeuroVol")) {
    cli::cli_abort("{.arg cluster_vol} must be a {.cls NeuroVol}.")
  }
  if (!inherits(surfatlas, "surfatlas")) {
    cli::cli_abort("{.arg surfatlas} must be a surface atlas ({.cls surfatlas}).")
  }
  if (is.null(surfatlas$lh_atlas) && is.null(surfatlas$rh_atlas)) {
    cli::cli_abort("{.arg surfatlas} has neither an lh nor an rh hemisphere.")
  }
  if (!requireNamespace("neurosurf", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg neurosurf} is required for surface projection.")
  }
  if (!is.numeric(surface_smooth_fwhm) || length(surface_smooth_fwhm) != 1L ||
      is.na(surface_smooth_fwhm) || surface_smooth_fwhm < 0) {
    cli::cli_abort("{.arg surface_smooth_fwhm} must be a non-negative number.")
  }

  out <- .project_cluster_overlay(
    cluster_vol = cluster_vol,
    surfatlas = surfatlas,
    space_override = space_override,
    density_override = density_override,
    resolution_override = resolution_override,
    fun = match.arg(fun),
    sampling = match.arg(sampling),
    interpolation = match.arg(interpolation),
    aggregate = aggregate,
    n_samples = n_samples,
    depth = depth,
    surface_smooth_fwhm = surface_smooth_fwhm
  )

  for (hemi in names(out$meta$hemis)) {
    err <- out$meta$hemis[[hemi]]$error
    if (!is.null(err)) {
      cli::cli_warn(c(
        "Projection onto the {hemi} hemisphere failed; its values are all NA.",
        "x" = "{err}"
      ))
    }
  }
  out
}

# Internal implementation shared by plot_brain(), the CPU renderer, and the
# cluster explorer. Kept under its historical name for callers that look it
# up directly; new code should use project_cluster_overlay().
.project_cluster_overlay <- function(cluster_vol,
                                     surfatlas,
                                     space_override = NULL,
                                     density_override = NULL,
                                     resolution_override = NULL,
                                     fun = c("avg", "nn", "mode"),
                                     sampling = c("midpoint",
                                                  "normal_line",
                                                  "thickness"),
                                     interpolation = c("legacy", "nearest",
                                                       "linear"),
                                     aggregate = NULL,
                                     n_samples = NULL,
                                     depth = NULL,
                                     surface_smooth_fwhm = 0) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  interpolation <- match.arg(interpolation)

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
      sampling = sampling,
      interpolation = interpolation,
      aggregate = aggregate,
      n_samples = n_samples,
      depth = depth,
      surface_smooth_fwhm = surface_smooth_fwhm
    )

    out[[hemi]] <- vals
    meta$hemis[[hemi]] <- list(
      target_vertices = target_n,
      projected_vertices = length(vals),
      finite_vertices = sum(is.finite(vals)),
      interpolation = interpolation,
      aggregate = aggregate,
      sampling = sampling,
      n_samples = n_samples,
      depth = depth,
      surface_smooth_fwhm = surface_smooth_fwhm
    )
    if (!is.null(attr(vals, "projection_error"))) {
      meta$hemis[[hemi]]$error <- attr(vals, "projection_error")
      attr(out[[hemi]], "projection_error") <- NULL
    }
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
                                      sampling,
                                      interpolation = "legacy",
                                      aggregate = NULL,
                                      n_samples = NULL,
                                      depth = NULL,
                                      surface_smooth_fwhm = 0) {
  failed <- function(reason) {
    structure(rep(NA_real_, target_n), projection_error = reason)
  }
  error <- NULL
  proj <- tryCatch(
    neurosurf::vol_to_surf(
      surf_wm = surf_wm,
      surf_pial = surf_pial,
      vol = cluster_vol,
      fun = fun,
      sampling = sampling,
      interpolation = interpolation,
      aggregate = aggregate,
      n_samples = n_samples,
      depth = depth,
      surface_smooth_fwhm = surface_smooth_fwhm,
      fill = NA_real_
    ),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }
  )
  if (!is.null(error)) {
    return(failed(error))
  }

  vals <- .surface_values_to_numeric(proj)
  if (is.null(vals)) {
    return(failed("neurosurf::vol_to_surf() returned no vertex values."))
  }

  if (length(vals) != target_n) {
    return(failed(sprintf(
      "Projected %d vertices but the atlas hemisphere has %d.",
      length(vals), target_n
    )))
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

#' Rebuild legacy SurfaceGeometry objects with current slot layout
#'
#' Older `data/fsaverage.rda` and bundled atlas geometries were serialized
#' before `neurosurf::SurfaceGeometry` gained the `label` and
#' `surf_to_world` slots. Accessing those slots on a legacy object errors
#' out, which causes `vol_to_surf()` to fail and emits all-NA overlays.
#' This helper validates an object and, if it is missing slots present in
#' the current class definition, reconstructs it via the public
#' constructor so every slot is populated. Returns the input unchanged if
#' it already validates or cannot be repaired.
#' @keywords internal
#' @noRd
.repair_legacy_surface_geometry <- function(g) {
  if (is.null(g) || !inherits(g, "SurfaceGeometry")) return(g)
  ok <- tryCatch({ methods::validObject(g); TRUE },
                 error = function(e) FALSE)
  if (ok) return(g)

  mesh <- tryCatch(g@mesh, error = function(e) NULL)
  if (is.null(mesh) || is.null(mesh$vb) || is.null(mesh$it)) return(g)

  vert <- t(mesh$vb[1:3, , drop = FALSE])
  faces <- t(mesh$it) - 1L
  storage.mode(faces) <- "integer"
  hemi <- tryCatch(g@hemi, error = function(e) NA_character_)
  if (length(hemi) != 1 || is.na(hemi) || !nzchar(hemi)) hemi <- "left"

  tryCatch(
    neurosurf::SurfaceGeometry(vert = vert, faces = faces, hemi = hemi),
    error = function(e) g
  )
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

  white <- .repair_legacy_surface_geometry(white)
  pial <- .repair_legacy_surface_geometry(pial)

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

  if (identical(surface_space, "fsaverage-std8") &&
      is.null(density_override) && is.null(resolution_override)) {
    return(neurosurf::load_fsaverage_std8(surface_type)[[hemi]])
  }

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

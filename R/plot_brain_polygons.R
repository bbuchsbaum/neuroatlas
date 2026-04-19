# Polygon builders used by plot_brain().
#
# These functions consume a surfatlas object plus requested views and
# produce polygon / boundary tibbles suitable for ggplot rendering.
# They sit one level above the pure geometry primitives in
# R/plot_brain_geometry.R (which they call into for normals, projection,
# boundary detection, and smoothing) and one level below the ggplot
# composition code in R/plot_brain.R.
#
# Split out from plot_brain.R; behaviour is intentionally unchanged.

#' Build merged polygon data for brain surface rendering
#'
#' Produces one polygon per parcel per view (instead of one per triangle),
#' dramatically reducing the number of SVG elements for interactive rendering.
#'
#' @param surfatlas A surfatlas object.
#' @param views Character vector of views to render.
#' @param surface Character: surface type.
#' @return A list with \code{polygons} and \code{boundaries} tibbles.
#' @keywords internal
#' @noRd
.build_merged_polygon_data <- function(surfatlas, views, surface,
                                       projection_smooth = 0L) {
  hemis <- c("left", "right")
  hemi_keys <- c(left = "lh", right = "rh")

  all_labels <- surfatlas$labels
  all_ids <- surfatlas$ids
  id_to_label <- stats::setNames(all_labels, as.character(all_ids))
  id_to_network <- NULL
  if (!is.null(surfatlas$network) &&
      length(surfatlas$network) == length(surfatlas$ids)) {
    id_to_network <- stats::setNames(as.character(surfatlas$network),
                                     as.character(surfatlas$ids))
  }

  polygon_list <- list()
  boundary_list <- list()
  global_offset <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]

    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, ])
    faces <- t(geom@mesh$it)
    parcel_vec <- as.integer(atlas_hemi@data)
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }

    fnormals <- .face_normals(verts, faces)
    fparcel <- .face_parcel_ids(parcel_vec, faces)

    for (v in views) {
      proj <- .project_view(verts, v, h)
      if (projection_smooth > 0L) {
        proj$xy <- .smooth_projected_xy(
          proj$xy,
          neighbors = neighbors,
          n_iter = projection_smooth
        )
      }
      vdir <- proj$view_dir

      dots <- fnormals[, 1] * vdir[1] +
              fnormals[, 2] * vdir[2] +
              fnormals[, 3] * vdir[3]
      visible <- which(dots > 0 & fparcel != 0)
      if (length(visible) == 0) next

      vis_faces  <- faces[visible, , drop = FALSE]
      vis_parcel <- fparcel[visible]
      panel_label <- paste0(tools::toTitleCase(h), " ",
                            tools::toTitleCase(v))

      # Merged parcel polygons
      merged <- .merge_parcel_polygons(vis_faces, vis_parcel, proj$xy,
                                       id_to_label)
      if (!is.null(merged)) {
        merged$poly_id <- merged$poly_id + global_offset
        global_offset <- max(merged$poly_id)
        merged$hemi  <- h
        merged$view  <- v
        merged$panel <- panel_label
        polygon_list[[length(polygon_list) + 1L]] <- merged
      }

      # Boundary edges for the border line layer
      bedge <- .compute_boundary_edges(vis_faces, vis_parcel, proj$xy,
                                       id_to_network = id_to_network)
      if (!is.null(bedge)) {
        bedge$panel <- panel_label
        boundary_list[[length(boundary_list) + 1L]] <- bedge
      }
    }
  }

  list(
    polygons   = dplyr::bind_rows(polygon_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}

#' Memoised version of .build_merged_polygon_data
#' @keywords internal
#' @noRd
.build_merged_polygon_data_memo <- memoise::memoise(.build_merged_polygon_data)


#' Build polygon data for brain surface rendering
#'
#' Generates a tibble of 2D projected polygon vertices for all requested
#' hemispheres and views, suitable for \code{ggplot2::geom_polygon}.
#'
#' @param surfatlas A surfatlas object (e.g. from \code{schaefer_surf()}).
#' @param views Character vector of views to render.
#' @param surface Character: surface type ("inflated", "pial", "white").
#' @return A list with two elements: \code{polygons} (tibble with columns x, y,
#'   face_id, vertex_order, parcel_id, hemi, view, panel, label) and
#'   \code{boundaries} (tibble with columns x, y, xend, yend, panel).
#' @keywords internal
#' @noRd
.build_brain_polygon_data <- function(surfatlas, views, surface,
                                      projection_smooth = 0L) {
  hemis <- c("left", "right")
  hemi_keys <- c(left = "lh", right = "rh")

  all_labels <- surfatlas$labels
  all_ids <- surfatlas$ids
  # Build label lookup: parcel_id -> label
  id_to_label <- stats::setNames(all_labels, as.character(all_ids))
  id_to_network <- NULL
  if (!is.null(surfatlas$network) &&
      length(surfatlas$network) == length(surfatlas$ids)) {
    id_to_network <- stats::setNames(as.character(surfatlas$network),
                                     as.character(surfatlas$ids))
  }

  polygon_list <- list()
  boundary_list <- list()
  global_face_id <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]

    # Extract geometry from the LabeledNeuroSurface
    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, ])   # N x 3
    faces <- t(geom@mesh$it)           # F x 3
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }

    # Per-vertex parcel IDs
    parcel_vec <- as.integer(atlas_hemi@data)

    # Compute face normals and per-face parcel IDs
    fnormals <- .face_normals(verts, faces)
    fparcel <- .face_parcel_ids(parcel_vec, faces)

    for (v in views) {
      proj <- .project_view(verts, v, h)
      if (projection_smooth > 0L) {
        proj$xy <- .smooth_projected_xy(
          proj$xy,
          neighbors = neighbors,
          n_iter = projection_smooth
        )
      }
      vdir <- proj$view_dir

      # Backface culling: keep faces whose normal dots positively with view dir
      dots <- fnormals[, 1] * vdir[1] + fnormals[, 2] * vdir[2] + fnormals[, 3] * vdir[3]
      visible <- which(dots > 0 & fparcel != 0)

      if (length(visible) == 0) next

      n_vis <- length(visible)
      fids <- global_face_id + seq_len(n_vis)
      global_face_id <- global_face_id + n_vis

      # Build 3-row-per-face tibble
      vis_faces <- faces[visible, , drop = FALSE]
      vis_parcel <- fparcel[visible]
      vis_shade <- dots[visible]

      x_coords <- c(proj$xy[vis_faces[, 1], 1],
                     proj$xy[vis_faces[, 2], 1],
                     proj$xy[vis_faces[, 3], 1])
      y_coords <- c(proj$xy[vis_faces[, 1], 2],
                     proj$xy[vis_faces[, 2], 2],
                     proj$xy[vis_faces[, 3], 2])

      panel_label <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))

      # Look up labels for these parcel IDs
      face_labels <- id_to_label[as.character(vis_parcel)]
      face_labels[is.na(face_labels)] <- ""

      chunk <- tibble::tibble(
        x = x_coords,
        y = y_coords,
        face_id = rep(fids, 3),
        vertex_order = rep(1:3, each = n_vis),
        parcel_id = rep(vis_parcel, 3),
        shade = rep(vis_shade, 3),
        hemi = h,
        view = v,
        panel = panel_label,
        label = rep(face_labels, 3)
      )
      polygon_list[[length(polygon_list) + 1L]] <- chunk

      # Compute parcel boundary edges for this view
      bedge <- .compute_boundary_edges(vis_faces, vis_parcel, proj$xy,
                                       id_to_network = id_to_network)
      if (!is.null(bedge)) {
        bedge$panel <- panel_label
        boundary_list[[length(boundary_list) + 1L]] <- bedge
      }
    }
  }

  list(
    polygons = dplyr::bind_rows(polygon_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}

#' Memoised version of .build_brain_polygon_data
#' @keywords internal
#' @noRd
.build_brain_polygon_data_memo <- memoise::memoise(.build_brain_polygon_data)

#' Build Surface Polygon Data for Rendering
#'
#' Build 2D projected polygon/boundary data from a \code{surfatlas} for custom
#' rendering workflows. This exposes the mesh-projection data pipeline used
#' internally by \code{\link{plot_brain}()}.
#'
#' @param surfatlas A surface atlas object inheriting from class
#'   \code{"surfatlas"}.
#' @param views Character vector of views to include. Any combination of
#'   \code{"lateral"}, \code{"medial"}, \code{"dorsal"}, \code{"ventral"}.
#' @param surface Character scalar identifying the surface type label.
#' @param merged Logical. If \code{TRUE} (default), returns merged
#'   parcel-level polygons (faster, fewer shapes). If \code{FALSE}, returns
#'   per-triangle polygons.
#' @param projection_smooth Non-negative integer controlling Laplacian-like
#'   smoothing iterations applied to projected vertex coordinates before
#'   polygon and boundary construction. \code{0} (default) keeps native mesh
#'   coordinates.
#' @param use_cache Logical. If \code{TRUE} (default), use memoized builders.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{polygons}}{A tibble of projected polygon vertices.}
#'   \item{\code{boundaries}}{A tibble of projected boundary segments.}
#' }
#' @export
build_surface_polygon_data <- function(surfatlas,
                                       views = c("lateral", "medial"),
                                       surface = "inflated",
                                       merged = TRUE,
                                       projection_smooth = 0L,
                                       use_cache = TRUE) {
  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.", call. = FALSE)
  }

  views <- match.arg(
    views,
    choices = c("lateral", "medial", "dorsal", "ventral"),
    several.ok = TRUE
  )

  if (!is.logical(merged) || length(merged) != 1 || is.na(merged)) {
    stop("'merged' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(use_cache) || length(use_cache) != 1 || is.na(use_cache)) {
    stop("'use_cache' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(projection_smooth) || length(projection_smooth) != 1 ||
      is.na(projection_smooth) || projection_smooth < 0 ||
      projection_smooth != as.integer(projection_smooth)) {
    stop("'projection_smooth' must be a non-negative integer scalar.",
         call. = FALSE)
  }
  projection_smooth <- as.integer(projection_smooth)

  if (isTRUE(merged)) {
    builder <- if (isTRUE(use_cache)) {
      .build_merged_polygon_data_memo
    } else {
      .build_merged_polygon_data
    }
  } else {
    builder <- if (isTRUE(use_cache)) {
      .build_brain_polygon_data_memo
    } else {
      .build_brain_polygon_data
    }
  }

  builder(
    surfatlas = surfatlas,
    views = views,
    surface = surface,
    projection_smooth = projection_smooth
  )
}

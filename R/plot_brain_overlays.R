# Overlay polygon construction for plot_brain().
#
# Builds per-face polygon data and value-to-colour mapping for
# continuous vertex-wise overlays rendered on top of the parcellation
# layer. Depends on the geometry primitives in R/plot_brain_geometry.R.
#
# Split out from plot_brain.R; behaviour is intentionally unchanged.

#' Build overlay face polygons from vertex-wise values
#'
#' @param surfatlas A surface atlas.
#' @param overlay A list with components \code{lh} and \code{rh} containing
#'   vertex-wise numeric vectors.
#' @param views Character vector of views.
#' @param hemis Character vector of hemispheres.
#' @param threshold Optional absolute threshold for overlay face values.
#' @return A list with \code{polygons} and \code{boundaries}.
#' @keywords internal
#' @noRd
.build_overlay_polygon_data <- function(surfatlas,
                                        overlay,
                                        views,
                                        hemis,
                                        projection_smooth = 0L,
                                        threshold = NULL) {
  hemi_keys <- c(left = "lh", right = "rh")
  poly_list <- list()
  boundary_list <- list()
  global_face_id <- 0L

  for (h in hemis) {
    hk <- hemi_keys[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]
    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, , drop = FALSE])
    faces <- t(geom@mesh$it)
    neighbors <- NULL
    if (projection_smooth > 0L) {
      neighbors <- .mesh_vertex_neighbors(faces, nrow(verts))
    }
    fnormals <- .face_normals(verts, faces)

    ov <- overlay[[hk]]
    if (is.null(ov)) next
    ov <- as.numeric(ov)
    if (length(ov) != nrow(verts)) next

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

      visible <- which(dots > 0)
      if (length(visible) == 0) next

      vis_faces <- faces[visible, , drop = FALSE]
      fvals <- rowMeans(
        cbind(
          ov[vis_faces[, 1]],
          ov[vis_faces[, 2]],
          ov[vis_faces[, 3]]
        ),
        na.rm = TRUE
      )

      finite <- is.finite(fvals)
      if (!is.null(threshold) && is.finite(threshold)) {
        finite <- finite & abs(fvals) >= threshold
      }

      panel_label <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))

      # Boundary between overlay-in and overlay-out regions
      face_bin <- integer(length(fvals))
      face_bin[finite] <- 1L
      if (any(face_bin == 1L)) {
        bedge <- .compute_boundary_edges(vis_faces, face_bin, proj$xy)
        if (!is.null(bedge)) {
          bedge <- bedge[bedge$edge_type == "parcel", , drop = FALSE]
          if (nrow(bedge) > 0) {
            bedge$panel <- panel_label
            boundary_list[[length(boundary_list) + 1L]] <- bedge
          }
        }
      }

      keep_faces <- which(finite)
      if (length(keep_faces) == 0) next

      vis_faces_keep <- vis_faces[keep_faces, , drop = FALSE]
      vals_keep <- fvals[keep_faces]
      n_keep <- nrow(vis_faces_keep)

      fids <- global_face_id + seq_len(n_keep)
      global_face_id <- global_face_id + n_keep

      chunk <- tibble::tibble(
        x = c(proj$xy[vis_faces_keep[, 1], 1],
              proj$xy[vis_faces_keep[, 2], 1],
              proj$xy[vis_faces_keep[, 3], 1]),
        y = c(proj$xy[vis_faces_keep[, 1], 2],
              proj$xy[vis_faces_keep[, 2], 2],
              proj$xy[vis_faces_keep[, 3], 2]),
        face_id = rep(fids, 3),
        panel = panel_label,
        overlay_value = rep(vals_keep, 3)
      )
      poly_list[[length(poly_list) + 1L]] <- chunk
    }
  }

  list(
    polygons = dplyr::bind_rows(poly_list),
    boundaries = dplyr::bind_rows(boundary_list)
  )
}


#' Map numeric overlay values to hex colours
#' @keywords internal
#' @noRd
.overlay_value_to_hex <- function(vals, palette = "vik", lim = NULL) {
  vals <- as.numeric(vals)
  finite <- is.finite(vals)
  out <- rep(NA_character_, length(vals))
  if (!any(finite)) return(out)

  if (is.null(lim)) {
    lim <- range(vals[finite], na.rm = TRUE)
    if (!all(is.finite(lim))) return(out)
    if (lim[1] == lim[2]) {
      lim <- lim + c(-1e-6, 1e-6)
    }
  }

  .require_suggest("scico", feature = "overlay colour mapping")
  pal <- scico::scico(256, palette = palette)
  v <- vals
  v[v < lim[1]] <- lim[1]
  v[v > lim[2]] <- lim[2]
  idx <- floor((v - lim[1]) / (lim[2] - lim[1]) * 255) + 1
  idx[!finite] <- NA_integer_
  out[finite] <- pal[idx[finite]]
  out
}

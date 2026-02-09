#' Compute ROI centroid coordinates from an atlas
#'
#' For each region in the atlas, computes the mean voxel coordinate and
#' converts to world (e.g. MNI) millimetre space using the volume's affine.
#'
#' @param atlas An S3 atlas object.
#' @return A \code{data.frame} with columns \code{id}, \code{x}, \code{y},
#'   \code{z} (one row per region).
#' @keywords internal
#' @noRd
.atlas_centroids <- function(atlas) {
  vol <- .get_atlas_volume(atlas)
  sp <- neuroim2::space(vol)
  ids <- atlas$ids

  # Extract voxel array

  if (methods::is(vol, "ClusteredNeuroVol")) {
    mask_idx <- which(vol@mask)
    grid <- neuroim2::index_to_grid(vol, mask_idx)
    vox_ids <- vol@clusters
  } else {
    arr <- vol[,,]
    mask_idx <- which(arr != 0)
    grid <- neuroim2::index_to_grid(vol, mask_idx)
    vox_ids <- arr[mask_idx]
  }

  # Convert grid to world coordinates
  world <- neuroim2::grid_to_coord(sp, grid)

  # Compute centroids per ROI
  centroids <- do.call(rbind, lapply(ids, function(rid) {
    sel <- which(vox_ids == rid)
    if (length(sel) == 0) {
      return(data.frame(id = rid, x = NA_real_, y = NA_real_, z = NA_real_))
    }
    data.frame(
      id = rid,
      x = mean(world[sel, 1]),
      y = mean(world[sel, 2]),
      z = mean(world[sel, 3])
    )
  }))

  centroids
}

#' Assign optimal colours to atlas regions
#'
#' Bridge between an atlas object and the \code{roi_colors_*()} family of
#' colour algorithms. Extracts ROI centroids, assembles a metadata tibble, and
#' dispatches to the requested algorithm.
#'
#' @param atlas An S3 atlas object (any type: Schaefer, ASEG, Olsen, etc.).
#' @param method One of \code{"rule_hcl"} (default), \code{"network_harmony"},
#'   \code{"maximin_view"}, \code{"embedding"}, or a named character vector of
#'   hex colours keyed by region ID.
#' @param ... Additional arguments passed to the underlying
#'   \code{roi_colors_*()} function.
#'
#' @return A \code{\link[tibble]{tibble}} with columns \code{id} (integer) and
#'   \code{color} (hex string), one row per region in the atlas.
#'
#' @details
#' When \code{method} is a character vector of hex colours (named by region ID
#' or in the same order as \code{atlas$ids}), no colour algorithm is invoked
#' and the colours are returned directly.
#'
#' @examples
#' \donttest{
#' atlas <- get_aseg_atlas()
#' cols <- atlas_roi_colors(atlas)
#' head(cols)
#' }
#'
#' @export
atlas_roi_colors <- function(atlas, method = "rule_hcl", ...) {
  # Manual colour vector path

  if (is.character(method) && length(method) > 1) {
    if (!is.null(names(method))) {
      # Named vector keyed by region ID
      cols <- method[as.character(atlas$ids)]
    } else {
      stopifnot(length(method) == length(atlas$ids))
      cols <- method
    }
    return(tibble::tibble(id = atlas$ids, color = unname(cols)))
  }

  method <- match.arg(method, c("rule_hcl", "network_harmony",
                                 "maximin_view", "embedding"))

  # Build the rois tibble expected by roi_colors_*()
  centroids <- .atlas_centroids(atlas)
  rois <- tibble::tibble(
    roi = centroids$id,
    x   = centroids$x,
    y   = centroids$y,
    z   = centroids$z
  )

  # Add optional metadata columns
  has_hemi <- !is.null(atlas$hemi) && !all(is.na(atlas$hemi))
  has_network <- !is.null(atlas$network) && !all(is.na(atlas$network))

  if (has_hemi) {
    rois$hemi <- atlas$hemi
  }
  if (has_network) {
    rois$network <- atlas$network
  }

  hemi_arg <- if (has_hemi) "hemi" else NULL
  network_arg <- if (has_network) "network" else NULL

  dots <- list(...)

  if (method == "rule_hcl") {
    pal <- do.call(roi_colors_rule_hcl, c(
      list(rois = rois, id_col = "roi", xyz_cols = c("x", "y", "z"),
           network_col = network_arg, hemi_col = hemi_arg),
      dots
    ))
  } else if (method == "network_harmony") {
    if (!has_network) {
      stop("method 'network_harmony' requires atlas$network to be non-NULL")
    }
    pal <- do.call(roi_colors_network_harmony, c(
      list(rois = rois, id_col = "roi", xyz_cols = c("x", "y", "z"),
           network_col = "network", hemi_col = hemi_arg),
      dots
    ))
  } else if (method == "maximin_view") {
    pal <- do.call(roi_colors_maximin_view, c(
      list(rois = rois, id_col = "roi", xyz_cols = c("x", "y", "z"),
           hemi_col = hemi_arg, network_col = network_arg),
      dots
    ))
  } else if (method == "embedding") {
    feature_cols <- if (!is.null(dots$feature_cols)) dots$feature_cols else c("x", "y", "z")
    dots$feature_cols <- NULL
    pal <- do.call(roi_colors_embedding, c(
      list(rois = rois, id_col = "roi", feature_cols = feature_cols,
           hemi_col = hemi_arg),
      dots
    ))
  }

  # Rename 'roi' column to 'id' for consistency
  names(pal)[names(pal) == "roi"] <- "id"
  pal
}

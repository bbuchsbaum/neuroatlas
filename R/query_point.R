#' Look Up Atlas Regions at MNI Coordinates
#'
#' @description
#' Given one or more 3D coordinates, look up the atlas region(s) at each
#' location. Supports exact voxel lookup (`radius = 0`) or fuzzy search
#' (`radius > 0`) that returns all regions within a sphere of given radius
#' in millimetres. Multiple atlases can be queried simultaneously.
#'
#' @param coords Numeric vector of length 3 (single point) or an N x 3 matrix
#'   of world coordinates.
#' @param atlas A single atlas object (class `"atlas"`) or a named list of
#'   atlas objects.
#' @param radius Numeric scalar giving the search radius in millimetres.
#'   `0` (default) performs an exact voxel lookup; values greater than zero
#'   return every region whose labelled voxels fall within `radius` mm of
#'   the query coordinate.
#' @param from_space Character string identifying the coordinate space of the
#'   input coordinates (default `"MNI152"`).
#'
#' @return A \code{\link[tibble]{tibble}} with columns:
#' \describe{
#'   \item{point}{Integer index of the query coordinate (row number).}
#'   \item{x, y, z}{The input world coordinates.}
#'   \item{atlas_name}{Name of the atlas.}
#'   \item{id}{Integer region ID (`NA` for background or out-of-bounds).}
#'   \item{label}{Region label string (`NA` for background/OOB).}
#'   \item{hemi}{Hemisphere designation (`NA` if unavailable).}
#'   \item{network}{Network label (`NA` if unavailable).}
#' }
#'
#' @details
#' World coordinates are converted to voxel grid positions via
#' \code{\link[neuroim2]{coord_to_grid}} and rounded to the nearest integer.
#' When `radius > 0`, a nearest-neighbour search is performed in world
#' coordinates using \code{\link[Rnanoflann]{nn}} to find all labelled atlas
#' voxels within `radius` mm.
#'
#' If the atlas carries a coordinate-space annotation
#' (`atlas$atlas_ref$coord_space`) that differs from `from_space`, the input
#' coordinates are transformed automatically via
#' \code{\link{transform_coords}}.
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' query_point(c(24, -10, 5), atlas)
#'
#' # Multiple points
#' pts <- rbind(c(24, -10, 5), c(-30, 20, 50))
#' query_point(pts, atlas)
#'
#' # Fuzzy search within 5 mm
#' query_point(c(24, -10, 5), atlas, radius = 5)
#'
#' # Query multiple atlases at once
#' atlases <- list(schaefer = atlas, aseg = get_aseg_atlas())
#' query_point(c(24, -10, 5), atlases)
#' }
#'
#' @importFrom neuroim2 space coord_to_grid grid_to_coord
#' @importFrom Rnanoflann nn
#' @importFrom tibble tibble
#' @export
query_point <- function(coords, atlas, radius = 0, from_space = "MNI152") {
  # --- Normalise coords to N x 3 matrix ---
  if (is.null(dim(coords))) {
    if (length(coords) != 3L) {
      stop("'coords' must be a length-3 vector or an N x 3 matrix")
    }
    coords <- matrix(coords, nrow = 1L)
  }
  coords <- as.matrix(coords)
  if (ncol(coords) != 3L) {
    stop("'coords' must have exactly 3 columns (x, y, z)")
  }
  n_points <- nrow(coords)


  # --- Normalise atlas to a named list ---
  if (inherits(atlas, "atlas")) {
    atlas_list <- stats::setNames(list(atlas), atlas$name %||% "atlas")
  } else if (is.list(atlas) && !inherits(atlas, "atlas")) {
    atlas_list <- atlas
    if (is.null(names(atlas_list))) {
      names(atlas_list) <- paste0("atlas_", seq_along(atlas_list))
    }
  } else {
    stop("'atlas' must be an atlas object or a named list of atlas objects")
  }

  # --- Query each atlas ---
  result_parts <- lapply(names(atlas_list), function(aname) {
    atlas_obj <- atlas_list[[aname]]
    .query_one_atlas(coords, atlas_obj, aname, radius, from_space)
  })

  do.call(rbind, result_parts)
}


# Internal: query a single atlas for all points
# Returns a tibble
#' @noRd
.query_one_atlas <- function(coords, atlas_obj, atlas_name, radius,
                             from_space) {
  n_points <- nrow(coords)


  # --- Get the volume ---
  vol <- .get_atlas_volume(atlas_obj)

  # --- Coordinate-space transform if needed ---
  query_coords <- coords
  if (!is.null(atlas_obj$atlas_ref$coord_space) &&
      !identical(toupper(atlas_obj$atlas_ref$coord_space),
                 toupper(from_space))) {
    query_coords <- transform_coords(
      coords,
      from = from_space,
      to = atlas_obj$atlas_ref$coord_space
    )
  }

  # --- Densify atlas volume to an array ---
  vol_dims <- dim(vol)
  if (methods::is(vol, "ClusteredNeuroVol")) {
    arr <- array(0L, dim = vol_dims)
    arr[which(vol@mask)] <- vol@clusters
  } else {
    arr <- as.integer(vol[, , ])
    dim(arr) <- vol_dims
  }

  if (radius == 0) {
    .query_exact(query_coords, vol, arr, atlas_obj, atlas_name)
  } else {
    .query_radius(query_coords, vol, arr, atlas_obj, atlas_name, radius)
  }
}


# Internal: exact voxel lookup (radius == 0)
#' @noRd
.query_exact <- function(coords, vol, arr, atlas_obj, atlas_name) {
  n_points <- nrow(coords)
  sp <- neuroim2::space(vol)
  vol_dims <- dim(vol)

  grid <- neuroim2::coord_to_grid(sp, coords)
  if (is.null(dim(grid))) {
    grid <- matrix(grid, nrow = 1L)
  }
  grid <- round(grid)

  # Build results row-by-row
  ids <- rep(NA_integer_, n_points)
  for (i in seq_len(n_points)) {
    gi <- grid[i, ]
    if (all(gi >= 1L) && gi[1] <= vol_dims[1] &&
        gi[2] <= vol_dims[2] && gi[3] <= vol_dims[3]) {
      val <- arr[gi[1], gi[2], gi[3]]
      if (!is.na(val) && val != 0L) {
        ids[i] <- val
      }
    }
  }

  .ids_to_tibble(ids, coords, atlas_obj, atlas_name)
}


# Internal: radius-based fuzzy search
#' @noRd
.query_radius <- function(coords, vol, arr, atlas_obj, atlas_name, radius) {
  n_points <- nrow(coords)
  sp <- neuroim2::space(vol)

  # Get world coordinates of all nonzero voxels
  nz <- which(arr != 0L, arr.ind = TRUE)
  if (nrow(nz) == 0L) {
    # No labelled voxels at all
    empty_ids <- rep(NA_integer_, n_points)
    return(.ids_to_tibble(empty_ids, coords, atlas_obj, atlas_name))
  }
  nz_world <- neuroim2::grid_to_coord(sp, nz)
  nz_labels <- arr[nz]

  # For each query point, find all labelled voxels within radius
  k_max <- min(nrow(nz), 200L)
  nn_res <- Rnanoflann::nn(data = nz_world, points = coords,
                           k = k_max)

  rows <- vector("list", n_points)
  for (i in seq_len(n_points)) {
    dists <- nn_res$distances[i, ]
    idxs <- nn_res$indices[i, ]
    within <- which(dists <= radius & dists < .Machine$double.xmax)
    if (length(within) == 0L) {
      rows[[i]] <- .ids_to_tibble(NA_integer_, coords[i, , drop = FALSE],
                                  atlas_obj, atlas_name)
    } else {
      hit_labels <- unique(nz_labels[idxs[within]])
      # One row per unique region found
      rows[[i]] <- .ids_to_tibble(hit_labels,
                                  coords[rep(i, length(hit_labels)), ,
                                         drop = FALSE],
                                  atlas_obj, atlas_name)
    }
  }

  do.call(rbind, rows)
}


# Internal: map integer IDs to label/hemi/network and build tibble
#' @noRd
.ids_to_tibble <- function(ids, coords, atlas_obj, atlas_name) {
  if (is.null(dim(coords))) {
    coords <- matrix(coords, nrow = 1L)
  }
  n <- length(ids)

  # Map IDs to metadata
  labels <- rep(NA_character_, n)
  hemis <- rep(NA_character_, n)
  networks <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    if (!is.na(ids[i])) {
      idx <- match(ids[i], atlas_obj$ids)
      if (!is.na(idx)) {
        labels[i] <- atlas_obj$labels[idx]
        if (!is.null(atlas_obj$hemi) && length(atlas_obj$hemi) >= idx) {
          hemis[i] <- atlas_obj$hemi[idx]
        }
        if (!is.null(atlas_obj$network) && length(atlas_obj$network) >= idx) {
          networks[i] <- atlas_obj$network[idx]
        }
      }
    }
  }

  # Determine point indices from coords matrix
  # For radius results that repeat the same point, use the row from the

  # original coords matrix (handled by caller via rep(i, ...))
  point_idx <- seq_len(nrow(coords))


  tibble::tibble(
    point = point_idx,
    x = coords[, 1],
    y = coords[, 2],
    z = coords[, 3],
    atlas_name = atlas_name,
    id = ids,
    label = labels,
    hemi = hemis,
    network = networks
  )
}

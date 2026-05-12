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
#' When `radius > 0`, candidate voxel centres in a local grid neighbourhood
#' are tested in world coordinates to find all labelled atlas voxels within
#' `radius` mm.
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
#' @importFrom tibble tibble
#' @export
query_point <- function(coords, atlas, radius = 0, from_space = "MNI152") {
  if (!is.numeric(radius) || length(radius) != 1L ||
      is.na(radius) || radius < 0) {
    stop("'radius' must be a non-negative numeric scalar")
  }

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

#' Query Atlas Labels by Coordinate
#'
#' @description
#' Atlas-first convenience wrappers for label lookup. `query_coord()` queries
#' world/mm coordinates, while `query_vox()` queries R-style 1-based voxel
#' grid indices and converts them to world coordinates before dispatching to
#' `query_point()`.
#'
#' @param x A single atlas object or a named list of atlas objects.
#' @param coords Numeric vector of length 3 or an N x 3 matrix of world/mm
#'   coordinates.
#' @param ijk Numeric/integer vector of length 3 or an N x 3 matrix of R-style
#'   1-based voxel grid indices.
#' @param ... Additional arguments passed to `query_point()`, such as
#'   `radius` or `from_space`.
#'
#' @return A tibble with atlas labels at the requested locations.
#' @export
query_coord <- function(x, coords, ...) {
  query_point(coords, x, ...)
}

#' @rdname query_coord
#' @export
query_vox <- function(x, ijk, ...) {
  if (inherits(x, "atlas")) {
    vol <- .get_atlas_volume(x)
  } else if (is.list(x) && !inherits(x, "atlas")) {
    if (length(x) == 0L || !inherits(x[[1]], "atlas")) {
      stop("'x' must be an atlas object or a named list of atlas objects")
    }
    vol <- .get_atlas_volume(x[[1]])
  } else {
    stop("'x' must be an atlas object or a named list of atlas objects")
  }

  if (is.null(dim(ijk))) {
    if (length(ijk) != 3L) {
      stop("'ijk' must be a length-3 vector or an N x 3 matrix")
    }
    ijk <- matrix(ijk, nrow = 1L)
  }
  ijk <- as.matrix(ijk)
  if (ncol(ijk) != 3L) {
    stop("'ijk' must have exactly 3 columns (i, j, k)")
  }

  coords <- neuroim2::grid_to_coord(neuroim2::space(vol), ijk)
  query_point(coords, x, ...)
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

  if (radius == 0) {
    .query_exact(query_coords, coords, vol, atlas_obj, atlas_name)
  } else {
    .query_radius(query_coords, coords, vol, atlas_obj, atlas_name, radius)
  }
}


# Internal: exact voxel lookup (radius == 0)
#' @noRd
.query_exact <- function(query_coords, output_coords, vol, atlas_obj,
                         atlas_name) {
  n_points <- nrow(query_coords)
  sp <- neuroim2::space(vol)

  grid <- neuroim2::coord_to_grid(sp, query_coords)
  if (is.null(dim(grid))) {
    grid <- matrix(grid, nrow = 1L)
  }
  grid <- round(grid)

  vals <- .atlas_values_at_grid(vol, grid)
  ids <- ifelse(!is.na(vals) & vals != 0L, vals, NA_integer_)

  .ids_to_tibble(
    ids,
    output_coords,
    atlas_obj,
    atlas_name,
    point = seq_len(n_points)
  )
}


# Internal: radius-based fuzzy search
#' @noRd
.query_radius <- function(query_coords, output_coords, vol, atlas_obj,
                          atlas_name, radius) {
  n_points <- nrow(query_coords)
  sp <- neuroim2::space(vol)
  vol_dims <- dim(vol)
  offsets <- .radius_grid_offsets(radius, sp)

  center_grid <- neuroim2::coord_to_grid(sp, query_coords)
  if (is.null(dim(center_grid))) {
    center_grid <- matrix(center_grid, nrow = 1L)
  }
  center_grid <- round(center_grid)

  rows <- vector("list", n_points)
  for (i in seq_len(n_points)) {
    cand_grid <- sweep(offsets, 2L, center_grid[i, ], "+")
    in_bounds <- .grid_in_bounds(cand_grid, vol_dims)
    cand_grid <- cand_grid[in_bounds, , drop = FALSE]

    if (nrow(cand_grid) == 0L) {
      rows[[i]] <- .ids_to_tibble(
        NA_integer_,
        output_coords[i, , drop = FALSE],
        atlas_obj,
        atlas_name,
        point = i
      )
      next
    }

    cand_world <- neuroim2::grid_to_coord(sp, cand_grid)
    dists <- sqrt(rowSums(
      (cand_world -
         matrix(query_coords[i, ], nrow(cand_world), 3L,
                byrow = TRUE))^2
    ))
    within <- dists <= radius + sqrt(.Machine$double.eps)
    cand_grid <- cand_grid[within, , drop = FALSE]

    if (nrow(cand_grid) == 0L) {
      rows[[i]] <- .ids_to_tibble(
        NA_integer_,
        output_coords[i, , drop = FALSE],
        atlas_obj,
        atlas_name,
        point = i
      )
      next
    }

    vals <- .atlas_values_at_grid(vol, cand_grid)
    hit_labels <- unique(vals[!is.na(vals) & vals != 0L])

    if (length(hit_labels) == 0L) {
      rows[[i]] <- .ids_to_tibble(
        NA_integer_,
        output_coords[i, , drop = FALSE],
        atlas_obj,
        atlas_name,
        point = i
      )
    } else {
      rows[[i]] <- .ids_to_tibble(
        hit_labels,
        output_coords[rep(i, length(hit_labels)), , drop = FALSE],
        atlas_obj,
        atlas_name,
        point = rep(i, length(hit_labels))
      )
    }
  }

  do.call(rbind, rows)
}


# Internal: local voxel offsets large enough to contain a world-space sphere.
#' @noRd
.radius_grid_offsets <- function(radius, sp) {
  spacing <- tryCatch(neuroim2::spacing(sp), error = function(e) NULL)
  spacing <- as.numeric(spacing)
  if (length(spacing) != 3L || any(!is.finite(spacing)) ||
      any(spacing <= 0)) {
    spacing <- rep(1, 3)
  }

  radius_vox <- as.integer(ceiling(radius / spacing)) + 1L
  grid <- expand.grid(
    i = seq.int(-radius_vox[1], radius_vox[1]),
    j = seq.int(-radius_vox[2], radius_vox[2]),
    k = seq.int(-radius_vox[3], radius_vox[3])
  )
  as.matrix(grid)
}


# Internal: values at R-style 1-based voxel grid coordinates.
#' @noRd
.atlas_values_at_grid <- function(vol, grid) {
  if (is.null(dim(grid))) {
    grid <- matrix(grid, nrow = 1L)
  }

  grid <- round(grid)
  vals <- rep(NA_integer_, nrow(grid))
  in_bounds <- .grid_in_bounds(grid, dim(vol))
  if (!any(in_bounds)) {
    return(vals)
  }

  query_grid <- grid[in_bounds, , drop = FALSE]
  vals[in_bounds] <- as.integer(vol[query_grid])

  vals
}


# Internal: in-bounds mask for R-style 1-based voxel grid coordinates.
#' @noRd
.grid_in_bounds <- function(grid, vol_dims) {
  if (nrow(grid) == 0L) {
    return(logical(0))
  }

  grid[, 1] >= 1L & grid[, 1] <= vol_dims[1] &
    grid[, 2] >= 1L & grid[, 2] <= vol_dims[2] &
    grid[, 3] >= 1L & grid[, 3] <= vol_dims[3]
}


# Internal: map integer IDs to label/hemi/network and build tibble
#' @noRd
.ids_to_tibble <- function(ids, coords, atlas_obj, atlas_name, point = NULL) {
  if (is.null(dim(coords))) {
    coords <- matrix(coords, nrow = 1L)
  }
  n <- length(ids)
  if (is.null(point)) {
    point <- seq_len(nrow(coords))
  }
  if (length(point) != n) {
    stop("'point' must have the same length as 'ids'")
  }

  # Map IDs to metadata
  labels <- rep(NA_character_, n)
  hemis <- rep(NA_character_, n)
  networks <- rep(NA_character_, n)

  idx <- match(ids, atlas_obj$ids)
  valid <- !is.na(ids) & !is.na(idx)
  if (any(valid)) {
    labels[valid] <- atlas_obj$labels[idx[valid]]

    if (!is.null(atlas_obj$hemi)) {
      hemi_valid <- valid & idx <= length(atlas_obj$hemi)
      hemis[hemi_valid] <- atlas_obj$hemi[idx[hemi_valid]]
    }

    if (!is.null(atlas_obj$network)) {
      network_valid <- valid & idx <= length(atlas_obj$network)
      networks[network_valid] <- atlas_obj$network[idx[network_valid]]
    }
  }

  out <- tibble::tibble(
    point = point,
    x = coords[, 1],
    y = coords[, 2],
    z = coords[, 3],
    atlas_name = atlas_name,
    id = ids,
    label = labels,
    hemi = hemis,
    network = networks
  )

  meta <- tryCatch(roi_metadata(atlas_obj), error = function(e) NULL)
  if (!is.null(meta)) {
    skip <- c(
      "id", "label", "label_full", "hemi", "network",
      "color_r", "color_g", "color_b",
      "template_space", "coord_space", "atlas_family", "atlas_model",
      "atlas_representation", "atlas_source", "atlas_confidence"
    )
    extra_cols <- setdiff(names(meta), skip)
    for (col in extra_cols) {
      vals <- rep(meta[[col]][NA_integer_], n)
      if (any(valid)) {
        vals[valid] <- meta[[col]][idx[valid]]
      }
      out[[col]] <- vals
    }
  }

  out
}

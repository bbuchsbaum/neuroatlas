#' Dilate Atlas Parcellation Boundaries
#'
#' @description
#' Expands the boundaries of brain atlas parcels by dilating them into adjacent
#' unassigned voxels within a specified mask. This is useful for filling small gaps
#' between parcels or extending parcels into neighboring regions.
#'
#' @details
#' The dilation process:
#' \itemize{
#'   \item Identifies unassigned voxels that lie within the mask
#'   \item For each unassigned voxel, finds assigned voxels within \code{radius}
#'     (Euclidean distance, in voxel units)
#'   \item Assigns the voxel to a parcel by inverse-distance-weighted voting over
#'     those neighbours (up to \code{maxn} of them)
#'   \item Leaves a voxel unassigned when no parcel lies within \code{radius},
#'     so distant in-mask voxels (e.g. cerebellar grey matter for a cortical
#'     atlas) are not absorbed
#'   \item Respects mask boundaries to prevent dilation into unwanted regions
#' }
#'
#' The function uses a k-d tree implementation (via Rnanoflann) for efficient nearest
#' neighbor searches in 3D space.
#'
#' @param atlas An object of class "atlas" containing the parcellation to be dilated
#' @param mask A binary mask (NeuroVol object) specifying valid voxels for dilation.
#'   Dilation will only occur within non-zero mask values. May also be a TemplateFlow
#'   space identifier (string) or list of `get_template()` arguments, which will be
#'   resolved to a NeuroVol via `.resolve_template_input()`.
#' @param radius Numeric. The maximum distance (in voxels) to search for neighboring
#'   parcels when dilating. Default: 4
#' @param maxn Integer. Maximum number of neighboring voxels to consider when
#'   determining parcel assignment. Default: 50
#'
#' @return A \code{ClusteredNeuroVol} object containing the dilated parcellation.
#'   The object maintains the original label mappings but may include additional
#'   voxels in existing parcels.
#'
#' @examples
#' \dontrun{
#' # Load an atlas
#' atlas <- get_aseg_atlas()
#'
#' # Create or load a brain mask
#' mask <- get_template_brainmask()
#' 
#' # Dilate the atlas within the mask
#' dilated <- dilate_atlas(atlas, mask, radius = 4)
#'
#' # More conservative dilation with fewer neighbors
#' dilated_conservative <- dilate_atlas(atlas, mask, radius = 2, maxn = 20)
#' }
#'
#' @seealso
#' \code{\link{get_template_brainmask}} for creating appropriate masks from TemplateFlow
#'
#' @references
#' The algorithm uses efficient k-d tree based nearest neighbor searches for
#' spatial queries in 3D voxel space.
#'
#' @importFrom assertthat assert_that
#' @importFrom neuroim2 index_to_coord index_to_grid ClusteredNeuroVol
#' @importFrom Rnanoflann nn
#' @export
dilate_atlas <- function(atlas, mask, radius = 4, maxn = 50) {
    # Validate and resolve inputs
    assertthat::assert_that(inherits(atlas, "atlas"),
                            msg = "`atlas` arg must be an atlas")
    assertthat::assert_that(radius > 0,
                            msg = "`radius` must be positive")
    assertthat::assert_that(maxn > 0,
                            msg = "`maxn` must be positive")

    # Allow TemplateFlow-style mask inputs (string/list) in addition to NeuroVol
    if (!inherits(mask, "NeuroVol")) {
      mask <- .resolve_template_input(mask, target_type = "NeuroVol")
    }
    assertthat::assert_that(inherits(mask, "NeuroVol"),
                            msg = "`mask` must be a NeuroVol object or resolvable via .resolve_template_input()")

    # Convert the atlas to a dense array
    # Convert ClusteredNeuroVol to regular NeuroVol
    atlas2 <- if (inherits(atlas$atlas, "ClusteredNeuroVol")) {
      # ClusteredNeuroVol needs special handling
      # Get the mask and clusters
      mask_indices <- which(atlas$atlas@mask)
      
      # Create a dense volume
      dense_array <- array(0, dim = dim(atlas$atlas))
      dense_array[mask_indices] <- atlas$atlas@clusters
      
      neuroim2::NeuroVol(dense_array, space = neuroim2::space(atlas$atlas))
    } else {
      atlas$atlas
    }

    # Dimension sanity check to avoid misaligned dilation
    assertthat::assert_that(all(dim(mask) == dim(atlas2)),
                            msg = "`mask` and `atlas` must have matching dimensions for dilation")

    # Identify the indices of labeled voxels and mask voxels
    atlas_idx <- which(atlas2 > 0)
    mask_idx <- which(as.logical(mask))
    diff_idx <- setdiff(mask_idx, atlas_idx)

    # Parcel label for each labeled voxel, aligned with the rows of grid_atlas
    # below. Indexed as a plain vector to avoid the S4 `[` slice semantics of
    # NeuroVol (a single index would otherwise return a whole slice).
    atlas_label_vec <- as.vector(atlas2)[atlas_idx]

    # Early return if there's nothing to dilate, or nothing to dilate *from*
    # (an atlas with no labeled voxels has no parcels to grow).
    if (length(diff_idx) == 0 || length(atlas_idx) == 0) {
        return(atlas)
    }

    # Convert linear indices to voxel-grid coordinates (voxel units; avoids anisotropy issues)
    grid_atlas <- neuroim2::index_to_grid(mask, atlas_idx)
    grid_diff  <- neuroim2::index_to_grid(mask, diff_idx)
    storage.mode(grid_atlas) <- "double"
    storage.mode(grid_diff) <- "double"
    search_radius <- radius  # already in voxel units

    # For very large point sets, process in chunks to avoid memory issues
    chunk_size <- 10000
    n_diff <- nrow(grid_diff)
    
    # Prepare output containers
    dims <- dim(atlas2)
    new_idx_accum <- integer(n_diff)
    new_label_accum <- integer(n_diff)
    write_pos <- 0L

    # Helper to process a chunk without retaining full neighbor matrices
    process_chunk <- function(start, end) {
        chunk_points <- grid_diff[start:end, , drop = FALSE]
        n_pts <- nrow(chunk_points)

        # Use a standard k-nearest-neighbour search and apply the radius cutoff
        # ourselves. Rnanoflann's "radius" search mode does not actually enforce
        # `radius` when `k` is fixed (it returns the k nearest points regardless
        # of distance, with unreliable distance values), so the standard search
        # is the only mode that yields correct Euclidean distances to filter on.
        k_use <- min(maxn, nrow(grid_atlas))
        chunk_ret <- Rnanoflann::nn(
            data   = grid_atlas,
            points = chunk_points,
            k      = k_use,
            search = "standard",
            square = FALSE,  # return Euclidean (not squared) distances to compare against `radius`
            sorted = TRUE
        )

        # Coerce to matrices so single-column (k == 1) results index uniformly.
        ind <- chunk_ret$indices
        dst <- chunk_ret$distances
        if (is.null(dim(ind))) ind <- matrix(ind, ncol = 1)
        if (is.null(dim(dst))) dst <- matrix(dst, ncol = 1)

        chosen_labels <- integer(n_pts)
        lin_indices   <- integer(n_pts)
        keep_n <- 0L

        for (j in seq_len(n_pts)) {
            neighbors <- ind[j, ]
            dists     <- dst[j, ]

            # Keep only real neighbours that fall within `radius` voxel units.
            # A voxel with no parcel inside the radius is left unassigned.
            in_rad <- neighbors > 0 & dists <= search_radius
            if (!any(in_rad)) next

            neighbors <- neighbors[in_rad]
            dists     <- dists[in_rad]

            # `neighbors` are row indices into grid_atlas, i.e. positions in
            # atlas_idx, so the aligned label vector maps them directly.
            neighbor_labels <- atlas_label_vec[neighbors]

            weights <- 1 / (dists + .Machine$double.eps)
            label_votes <- tapply(weights, neighbor_labels, sum)
            lbl <- as.numeric(names(which.max(label_votes)))

            g2 <- grid_diff[start + j - 1, , drop = FALSE]
            keep_n <- keep_n + 1L
            chosen_labels[keep_n] <- lbl
            lin_indices[keep_n]   <- (g2[, 3] - 1) * dims[1] * dims[2] +
                                     (g2[, 2] - 1) * dims[1] + g2[, 1]
        }

        if (keep_n == 0L) return(NULL)
        list(idx = lin_indices[seq_len(keep_n)],
             lbl = chosen_labels[seq_len(keep_n)])
    }

    if (n_diff > chunk_size) {
        for (start in seq(1, n_diff, by = chunk_size)) {
            end <- min(start + chunk_size - 1, n_diff)
            chunk_res <- process_chunk(start, end)
            if (!is.null(chunk_res)) {
                len <- length(chunk_res$idx)
                tgt <- (write_pos + 1L):(write_pos + len)
                new_idx_accum[tgt]   <- chunk_res$idx
                new_label_accum[tgt] <- chunk_res$lbl
                write_pos <- write_pos + len
            }
        }
    } else {
        chunk_res <- process_chunk(1, n_diff)
        if (!is.null(chunk_res)) {
            len <- length(chunk_res$idx)
            tgt <- (write_pos + 1L):(write_pos + len)
            new_idx_accum[tgt]   <- chunk_res$idx
            new_label_accum[tgt] <- chunk_res$lbl
            write_pos <- write_pos + len
        }
    }

    if (write_pos == 0L) {
        return(atlas)
    }

    new_idx_accum <- new_idx_accum[seq_len(write_pos)]
    new_label_accum <- new_label_accum[seq_len(write_pos)]

    # Assign chosen labels
    atlas2[new_idx_accum] <- new_label_accum

    # Get label_map if available
    label_map <- if (inherits(atlas$atlas, "ClusteredNeuroVol") && !is.null(atlas$atlas@label_map)) {
        atlas$atlas@label_map
    } else {
        # Create a default label map from the atlas IDs
        lm <- as.list(atlas$ids)
        names(lm) <- as.character(atlas$ids)
        lm
    }
    
    # Check that we haven't introduced any labels not present in original atlas
    unique_labels <- unique(atlas2[atlas2 != 0])
    original_labels <- if (!is.null(atlas$ids)) {
        atlas$ids
    } else if (inherits(atlas$atlas, "ClusteredNeuroVol")) {
        # For ClusteredNeuroVol, get unique cluster values
        unique(atlas$atlas@clusters)
    } else {
        # For regular NeuroVol
        unique(atlas$atlas[atlas$atlas != 0])
    }
    missing_labels <- setdiff(unique_labels, original_labels)
    if (length(missing_labels) > 0) {
        stop(sprintf(
            "Found labels in dilated atlas that are not in original: %s",
            paste(missing_labels, collapse = ", ")
        ))
    }

    # Create the new dilated ClusteredNeuroVol
    # First create a LogicalNeuroVol mask
    mask_vol <- neuroim2::LogicalNeuroVol(atlas2 != 0, space = neuroim2::space(atlas2))
    
    dilated_vol <- neuroim2::ClusteredNeuroVol(
        mask    = mask_vol,
        clusters = atlas2[atlas2 != 0],
        label_map = label_map
    )

    # Now return an atlas object (with the same fields/classes as the input).
    # We replace only the $atlas slot with the dilated volume.
    new_atlas <- atlas
    new_atlas$atlas <- dilated_vol

    # Preserve the class of the original atlas
    class(new_atlas) <- class(atlas)

    new_atlas
}

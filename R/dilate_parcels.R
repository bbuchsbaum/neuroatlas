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
#'   \item Identifies unassigned voxels within the mask that are adjacent to existing parcels
#'   \item For each unassigned voxel, finds nearby assigned voxels within the specified radius
#'   \item Assigns the unassigned voxel to the nearest parcel
#'   \item Respects mask boundaries to prevent dilation into unwanted regions
#' }
#'
#' The function uses a k-d tree implementation (via rflann) for efficient nearest
#' neighbor searches in 3D space.
#'
#' @param atlas An object of class "atlas" containing the parcellation to be dilated
#' @param mask A binary mask (NeuroVol object) specifying valid voxels for dilation.
#'   Dilation will only occur within non-zero mask values.
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
#' # Create a brain mask
#' mask <- create_brain_mask(atlas)
#'
#' # Dilate parcels by 4 voxels
#' dilated <- dilate_atlas(atlas, mask, radius = 4)
#'
#' # More conservative dilation with fewer neighbors
#' dilated_conservative <- dilate_atlas(atlas, mask, radius = 2, maxn = 20)
#' }
#'
#' @seealso
#' \code{\link{create_brain_mask}} for creating appropriate masks
#'
#' @references
#' The algorithm uses the FLANN library for efficient nearest neighbor searches:
#' Muja, M., & Lowe, D. G. (2014). Scalable nearest neighbor algorithms for high
#' dimensional data. IEEE Transactions on Pattern Analysis and Machine Intelligence,
#' 36(11), 2227-2240.
#'
#' @importFrom assertthat assert_that
#' @importFrom neuroim2 index_to_coord index_to_grid ClusteredNeuroVol
#' @importFrom rflann RadiusSearch
#' @export
dilate_atlas <- function(atlas, mask, radius = 4, maxn = 50) {
    # Validate inputs
    assertthat::assert_that(inherits(atlas, "atlas"),
                            msg = "`atlas` arg must be an atlas")
    assertthat::assert_that(inherits(mask, "NeuroVol"),
                            msg = "`mask` must be a NeuroVol object")
    assertthat::assert_that(radius > 0,
                            msg = "`radius` must be positive")
    assertthat::assert_that(maxn > 0,
                            msg = "`maxn` must be positive")

    # Convert the atlas to a dense array
    atlas2 <- neuroim2::as.dense(atlas$atlas)

    # Identify the indices of labeled voxels and mask voxels
    atlas_idx <- which(atlas2 > 0)
    mask_idx <- which(as.logical(mask))
    diff_idx <- setdiff(mask_idx, atlas_idx)

    # Early return if there's nothing to dilate
    if (length(diff_idx) == 0) {
        return(atlas)
    }

    # Convert linear indices to coordinates
    cd_atlas <- neuroim2::index_to_coord(mask, atlas_idx)
    cd_diff <- neuroim2::index_to_coord(mask, diff_idx)

    # Perform neighbor search for unlabeled (diff) voxels within the specified radius
    ret <- rflann::RadiusSearch(cd_diff, cd_atlas, radius = radius, max_neighbour = maxn)

    # Indices of points that have any neighbors
    qlen <- sapply(ret$indices, length)
    valid_queries <- which(qlen > 0)
    if (length(valid_queries) == 0) {
        # No neighbors for unlabeled voxels, so no change
        return(atlas)
    }

    # Filter neighbor sets to only those with valid neighbors
    indset <- ret$indices[valid_queries]
    dset   <- ret$distances[valid_queries]

    # Prepare output: list form, then we rbind later
    out_list <- vector("list", length(valid_queries))

    # Grid (x,y,z) for the labeled vs unlabeled sets
    dims <- dim(atlas2)
    grid_atlas <- neuroim2::index_to_grid(mask, atlas_idx)
    grid_diff  <- neuroim2::index_to_grid(mask, diff_idx)

    # For each unlabeled voxel that has neighbors
    for (i in seq_along(valid_queries)) {
        voxel_idx <- valid_queries[i]
        neighbors <- indset[[i]]
        distances <- dset[[i]]

        # Grid coords of the unlabeled voxel we're filling
        g2 <- grid_diff[voxel_idx, , drop = FALSE]

        # The neighbor indices are indices into cd_atlas, which map back to atlas_idx
        neighbor_lin_idx <- atlas_idx[neighbors]
        neighbor_labels  <- atlas2[neighbor_lin_idx]

        # Weight by inverse distance
        weights <- 1 / (distances + .Machine$double.eps)
        label_votes <- tapply(weights, neighbor_labels, sum)
        chosen_label <- as.numeric(names(which.max(label_votes)))

        # Store (x, y, z, chosen_label)
        out_list[[i]] <- cbind(g2, chosen_label)
    }

    # Combine rows
    out_df <- do.call(rbind, out_list)

    # Convert (x,y,z) back to linear indices
    sub2ind <- function(sz, xyz) {
        # xyz is Nx3
        (xyz[,3] - 1) * sz[1] * sz[2] + (xyz[,2] - 1) * sz[1] + xyz[,1]
    }
    new_lin_idx <- sub2ind(dims, out_df[, 1:3, drop = FALSE])

    # Assign chosen labels
    atlas2[new_lin_idx] <- out_df[, 4]

    # Check that we haven't introduced any labels not present in label_map
    unique_labels <- unique(atlas2[atlas2 != 0])
    missing_labels <- setdiff(unique_labels, unlist(atlas$atlas@label_map))
    if (length(missing_labels) > 0) {
        stop(sprintf(
            "Found labels in dilated atlas that are not in label_map: %s",
            paste(missing_labels, collapse = ", ")
        ))
    }

    # Create the new dilated ClusteredNeuroVol
    dilated_vol <- neuroim2::ClusteredNeuroVol(
        mask    = as.logical(atlas2),
        clusters = atlas2[atlas2 != 0],
        label_map = atlas$atlas@label_map
    )

    # Now return an atlas object (with the same fields/classes as the input).
    # We replace only the $atlas slot with the dilated volume.
    new_atlas <- atlas
    new_atlas$atlas <- dilated_vol

    # Preserve the class of the original atlas
    class(new_atlas) <- class(atlas)

    new_atlas
}

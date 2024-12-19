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
assertthat::assert_that(inherits(atlas, "atlas"), msg = "`atlas` arg must be an atlas")
    assertthat::assert_that(inherits(mask, "NeuroVol"), msg = "`mask` must be a NeuroVol object")
    assertthat::assert_that(radius > 0, msg = "radius must be positive")
    assertthat::assert_that(maxn > 0, msg = "maxn must be positive")
    atlas2 <- as.dense(atlas$atlas)
    atlas_idx <- which(atlas2 > 0)
    mask_idx <- which(mask > 0)
    diff_idx <- setdiff(mask_idx, atlas_idx)
    if (length(diff_idx) == 0) {
        return(atlas$atlas)
    }
    cd1 <- index_to_coord(mask, atlas_idx)
    cd2 <- index_to_coord(mask, diff_idx)
    grid1 <- index_to_grid(mask, atlas_idx)
    grid2 <- index_to_grid(mask, diff_idx)
    ret <- rflann::RadiusSearch(cd2, cd1, radius = radius, max_neighbour = maxn)
    qlen <- sapply(ret$indices, function(x) length(x))
    indset <- ret$indices[qlen > 0]
    indices <- which(qlen > 0)
    dset <- ret$distances[qlen > 0]
    out <- vector(length(indices), mode = "list")
    for (i in 1:length(indices)) {
        ind <- indset[[i]]
        g <- grid1[ind, , drop = FALSE]
        g2 <- grid2[indices[i], , drop = FALSE]
        neighbor_labels <- atlas2[g]
        neighbor_distances <- dset[[i]]
        weights <- 1/(neighbor_distances + .Machine$double.eps)
        label_votes <- tapply(weights, neighbor_labels, sum)
        chosen_label <- as.numeric(names(which.max(label_votes)))
        out[[i]] <- cbind(g2, chosen_label)
    }
    out <- do.call(rbind, out)
    atlas2[out[, 1:3]] <- out[, 4]
    unique_labels <- unique(atlas2[atlas2 != 0])
    missing_labels <- setdiff(unique_labels, as.numeric(names(atlas2@label_map)))
    if (length(missing_labels) > 0) {
        stop(sprintf("Found labels in dilated atlas that are not in label_map: %s", 
            paste(missing_labels, collapse = ", ")))
    }
    neuroim2::ClusteredNeuroVol(as.logical(atlas2), clusters = atlas2[atlas2 != 
        0], label_map = atlas2@label_map)
}
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
dilate_parcels <- function(atlas, mask, radius = 4, maxn = 50) {
    assertthat::assert_that(inherits(atlas, "atlas"), msg = "`atlas` arg must be an atlas")
    assertthat::assert_that(inherits(mask, "NeuroVol"), msg = "`mask` must be a NeuroVol object")
    assertthat::assert_that(radius > 0, msg = "radius must be positive")
    assertthat::assert_that(maxn > 0, msg = "maxn must be positive")
    
    # Convert atlas to a dense array (3D), assuming as.dense() returns a plain numeric array
    atlas2 <- as.dense(atlas$atlas)
    
    atlas_idx <- which(atlas2 > 0)
    mask_idx <- which(as.logical(mask))
    diff_idx <- setdiff(mask_idx, atlas_idx)
    if (length(diff_idx) == 0) {
        return(atlas$atlas)  # no dilation needed
    }
    
    # Convert indices to coordinates
    cd_atlas <- index_to_coord(mask, atlas_idx)  # Nx3 coords of labeled voxels
    cd_diff <- index_to_coord(mask, diff_idx)    # Mx3 coords of unlabeled voxels to fill
    
    # Use RadiusSearch to find neighbors within radius
    # cd_diff: query points, cd_atlas: database points
    ret <- rflann::RadiusSearch(cd_diff, cd_atlas, radius = radius, max_neighbour = maxn)
    
    qlen <- sapply(ret$indices, length)
    have_neighbors <- which(qlen > 0)
    if (length(have_neighbors) == 0) {
        # No neighbors found, no change
        return(atlas$atlas)
    }
    
    indset <- ret$indices[have_neighbors]
    dset <- ret$distances[have_neighbors]
    
    # Pre-allocate result matrix: [x, y, z, chosen_label]
    # We'll fill only for voxels that had neighbors
    out <- matrix(NA_real_, nrow = length(have_neighbors), ncol = 4)
    
    # Convert atlas coordinates to linear indices for fast labeling lookups
    # We'll need a function to convert from (x,y,z) to linear indices
    dims <- dim(atlas2)
    sub2ind <- function(sz, x, y, z) {
        (z - 1) * sz[1] * sz[2] + (y - 1) * sz[1] + x
    }
    
    # Precompute linear indices for atlas points
    atlas_lin_idx <- atlas_idx  # we already have them
    # atlas_idx are linear indices. For neighbors, we have coordinates, so to get labels:
    # We'll also need to do this for each neighbor voxel coordinate set
    
    # Loop over diff voxels that have neighbors
    for (i in seq_along(have_neighbors)) {
        voxel_idx <- have_neighbors[i]
        # Coordinates of voxel to fill
        vx_coord <- cd_diff[voxel_idx, ]
        
        # Neighbor indices (in atlas_idx order)
        neighbors <- indset[[i]]
        distances <- dset[[i]]
        
        # Get the corresponding atlas linear indices of these neighbors
        # neighbors are indices into cd_atlas, which correspond directly to atlas_idx
        neighbor_lin_idx <- atlas_idx[neighbors]
        
        # Retrieve neighbor labels
        neighbor_labels <- atlas2[neighbor_lin_idx]
        
        # Compute weights = 1/(distance+eps)
        weights <- 1/(distances + .Machine$double.eps)
        
        # Compute weighted vote
        # If labels are integers, we can use tapply or a more optimized approach:
        label_votes <- tapply(weights, neighbor_labels, sum)
        
        chosen_label <- as.numeric(names(which.max(label_votes)))
        
        out[i, ] <- c(vx_coord, chosen_label)
    }
    
    # Now assign new labels to these diff voxels
    # Convert their coordinates to linear indices
    new_lin_idx <- sub2ind(dims, out[,1], out[,2], out[,3])
    atlas2[new_lin_idx] <- out[,4]
    
    # Check for missing labels
    unique_labels <- unique(atlas2[atlas2 != 0])
    missing_labels <- setdiff(unique_labels, as.numeric(names(atlas2@label_map)))
    if (length(missing_labels) > 0) {
        stop(sprintf("Found labels in dilated atlas that are not in label_map: %s", 
                     paste(missing_labels, collapse = ", ")))
    }
    
    # Return a new ClusteredNeuroVol
    # Make sure to reconstruct properly from atlas2
    # atlas2 is now a numeric array with labels
    mask_vol <- atlas2 != 0
    neuroim2::ClusteredNeuroVol(mask = as.logical(mask_vol),
                                clusters = atlas2[mask_vol],
                                label_map = atlas2@label_map)
}
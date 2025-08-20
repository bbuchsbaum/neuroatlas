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
#' The function uses a k-d tree implementation (via Rnanoflann) for efficient nearest
#' neighbor searches in 3D space.
#'
#' @param atlas An object of class "atlas" containing the parcellation to be dilated
#' @param mask A binary mask (NeuroVol object) specifying valid voxels for dilation.
#'   Dilation will only occur within non-zero mask values. Typically a LogicalNeuroVol,
#'   but can be any NeuroVol that will be converted to logical via as.logical().
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

    # Get voxel dimensions to scale radius appropriately
    voxel_dims <- neuroim2::spacing(mask)
    # Scale radius by the minimum voxel dimension to get appropriate search radius in mm
    search_radius <- radius * min(voxel_dims)

    # For very large point sets, process in chunks to avoid memory issues
    chunk_size <- 10000
    n_diff <- nrow(cd_diff)
    
    if (n_diff > chunk_size) {
        # Process in chunks
        all_indices <- list()
        all_distances <- list()
        
        for (start in seq(1, n_diff, by = chunk_size)) {
            end <- min(start + chunk_size - 1, n_diff)
            chunk_points <- cd_diff[start:end, , drop = FALSE]
            
            chunk_ret <- Rnanoflann::nn(data = cd_atlas, 
                                       points = chunk_points, 
                                       k = maxn,
                                       search = "radius", 
                                       radius = search_radius,
                                       sorted = TRUE)
            
            all_indices[[length(all_indices) + 1]] <- chunk_ret$indices
            all_distances[[length(all_distances) + 1]] <- chunk_ret$distances
        }
        
        # Combine results
        ret <- list(
            indices = do.call(rbind, all_indices),
            distances = do.call(rbind, all_distances)
        )
    } else {
        # Process all at once for smaller datasets
        ret <- Rnanoflann::nn(data = cd_atlas, 
                              points = cd_diff, 
                              k = maxn,
                              search = "radius", 
                              radius = search_radius,
                              sorted = TRUE)
    }

    # The nn() function returns matrices, need to convert to list format
    # ret$indices is a matrix where each row contains the indices of neighbors
    # ret$distances is a matrix where each row contains the distances to neighbors
    
    # Convert to list format and filter out points with no neighbors
    # In radius search, points with no neighbors have indices = 0
    valid_queries <- which(ret$indices[,1] != 0)
    
    if (length(valid_queries) == 0) {
        # No neighbors for unlabeled voxels, so no change
        return(atlas)
    }

    # Convert matrix output to list format for compatibility with existing code
    indset <- lapply(valid_queries, function(i) {
        idx <- ret$indices[i,]
        idx[idx != 0]  # Remove zeros (no neighbor indicators)
    })
    
    dset <- lapply(valid_queries, function(i) {
        idx <- ret$indices[i,]
        dist <- ret$distances[i,]
        dist[idx != 0]  # Keep only valid distances
    })

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

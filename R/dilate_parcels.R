

#' @param atlas the brain atlas
#' @param mask the mask of voxels to include for potential dilation 
#' @param radius the dilation expansion radius
#' @param maxn the maximum number of neighbors to consider for dilation
#' @export
dilate_atlas <- function(atlas, mask, radius=4, maxn=50) {
  assertthat::assert_that(inherits(atlas, "atlas"), msg="`atlas` arg must be an atlas")
  atlas2 <- as.dense(atlas$atlas)
  atlas_idx <- which(atlas$atlas > 0)
  mask_idx <-  which(mask>0)
  diff_idx <- setdiff(mask_idx, atlas_idx)
  
  cd1 <- index_to_coord(mask, atlas_idx)
  cd2 <- index_to_coord(mask,diff_idx)
  
  grid1 <- index_to_grid(mask, atlas_idx)
  grid2 <- index_to_grid(mask, diff_idx)
  
  ret <- rflann::RadiusSearch(cd2, cd1, radius=radius, max_neighbour=maxn)
  
  qlen <- sapply(ret$indices, function(x) length(x))
  indset <- ret$indices[qlen > 0]
  indices <- which(qlen>0)
  dset <- ret$distances[qlen > 0]
  
  out <- vector(length(indices), mode="list")
  for (i in 1:length(indices)) {
    ind <- indset[[i]]
    g <- grid1[ind,,drop=FALSE]
    g2 <- grid2[indices[i],,drop=FALSE]
    out[[i]] <- cbind(g2, atlas2[g][1])
  }
  
  out <- do.call(rbind, out)
  atlas2[out[,1:3]] <- out[,4]
  
  newatl <- neuroim2::ClusteredNeuroVol(as.logical(atlas2), 
                                        clusters=atlas2[atlas2!=0], 
                                        label_map=atlas2@label_map)
  
  
}
## this requires dilating the mask, then expanding clusters to dilated part
# 
# dilate_parcels <- function(atlas, mask, radius=NULL) {
#   
#   ds <- spacing(mask)
#   vol <- NeuroVol(as.vector(atlas$atlas@data), space(atlas$atlas@mask))
#   
#   if (is.null(radius)) {
#     radius=max(ds)+.1
#   }
# 
#   sl <- neuroim2::searchlight_coords(mask, radius=radius, nonzero=FALSE)
#   
#   for (i in 1:length(sl)) {
#     cds <- sl[[i]]
# 
#     if (nrow(cds) > 2) {
#       labels <- vol[cds]
#       if (all(labels[1] != labels[2:length(labels)])) {
#         md <- getmode(labels)
# 
#         if (md != 0) {
#           vol2[cds[1,,drop=FALSE]] <- md
#         }
#       }
#     }
#   }
# 
#   vol2[mask == 0] <- 0
#   vol <- vol2
# 
# }
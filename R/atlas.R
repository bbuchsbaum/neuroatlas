



#' @export
print.atlas <- function(x) {
  cat("Atlas name:", x$name, "\n")
  cat("Number of regions: ", length(x$ids), "\n\n")
  print(x$atlas)
}
  

#' @keywords internal
#' @noRd
create_cache_dir <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  if (!dir.exists(dname)) {
    dir.create(dname)
  }
  dname
}

#' @keywords internal
#' @noRd
get_cache_dir <- function() {
  create_cache_dir()
  #paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
}

#' @keywords internal
#' @noRd
clear_cache <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  fnames <- list.files(dname, full.names=TRUE)
  sapply(fnames, unlink)
}


#' Merge Atlases
#'
#' This function merges two atlases into a single atlas object, combining their
#' labels, color maps, ids, and hemispheres.
#'
#' @param atlas1 The first atlas object to merge.
#' @param atlas2 The second atlas object to merge.
#'
#' @return A list containing the merged atlas information:
#'   \itemize{
#'     \item{name}{The concatenated names of the input atlases, separated by "::".}
#'     \item{atlas}{A ClusteredNeuroVol object representing the merged atlas.}
#'     \item{cmap}{A matrix containing the color maps of both atlases.}
#'     \item{ids}{A vector containing the combined ids of both atlases.}
#'     \item{labels}{A vector containing the combined labels of both atlases.}
#'     \item{orig_labels}{A vector containing the combined original labels of both atlases.}
#'     \item{hemi}{A vector containing the combined hemispheres of both atlases.}
#'   }
#' @export
#' @examples
#' atlas1 <- get_aseg_atlas()
#' atlas2 <- get_aseg_atlas()
#' merged_atlas <- merge_atlases(atlas1, atlas2)
merge_atlases <- function(atlas1, atlas2) {
  assertthat::assert_that(all(dim(atlas1$atlas) == dim(atlas2$atlas)))

  atl2 <- atlas2$atlas
  atl2[atl2 != 0] <- atl2[atl2 != 0] + max(atlas1$ids) 
  atlmerged <- neuroim2::NeuroVol(as.numeric(atlas1$atlas@data), space=space(atlas1$atlas))
  
  
  atlmerged[atl2 != 0] <- atl2[atl2 != 0]
  
  #vol <- neuroim2::ClusteredNeuroVol(as.logical(vol), clusters=vol[vol!=0], 
  #                                   label_map=label_map)
  
  ids <- atlmerged[atlmerged != 0]
  cids <- 1:length(unique(ids))
  label_map <- as.list(cids)
  names(label_map) <- c(atlas1$orig_labels, atlas2$orig_labels)
  
  atlmerged <- neuroim2::ClusteredNeuroVol(as.logical(atlmerged),
                                           clusters=atlmerged[atlmerged != 0],
                                           label_map=label_map)
                                           
  

  ret <- list(
    name=paste0(atlas1$name,"::", atlas2$name),
    atlas=atlmerged,
    cmap=rbind(atlas1$cmap, atlas2$cmap),
    ids=c(atlas1$ids, atlas2$ids + max(atlas1$ids) + 1),
    labels=c(atlas1$labels, atlas2$labels),
    orig_labels=c(atlas1$orig_labels, atlas2$orig_labels),
    hemi=c(atlas1$hemi, atlas2$hemi)
  )

  class(ret) <- c(paste0(atlas1$name,"::", atlas2$name), "atlas")
  ret
}


#' @importFrom neuroim2 space ROIVol index_to_grid
get_roi.atlas <- function(x, label, id=NULL, hemi=NULL) {
  if (!is.null(label) && !is.null(id)) {
    stop("must supply one of 'id' or 'label' but not both")
  }

  if (!is.null(label)) {
    ret <- lapply(label, function(l) {
      id <- x$ids[which(x$labels == l)]
      if (length(id) == 0) {
        stop(paste0("label '", l, "' not found in atlas"))
      }
      rind <- which(x$atlas %in% id)
      neuroim2::ROIVol(neuroim2::space(x$atlas),
                       coords = neuroim2::index_to_grid(x$atlas, rind),
                       data=x$atlas[rind])
    })

    names(ret) <- label
    ret
  } else {
    ret <- lapply(id, function(i) {
      rind <- which(x$atlas %in% i)
      neuroim2::ROIVol(neuroim2::space(x$atlas),coords = neuroim2::index_to_grid(x$atlas, rind),data=i)
    })
    names(ret) <- id
    ret
  }
}



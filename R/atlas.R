get_roi.atlas <- function(x, label, id=NULL, hemi=NULL) {
  if (!is.null(label) && !is.null(id)) {
    stop("must supply one of 'id' or 'label' but not both")
  }
  
  if (!is.null(label)) {
    ret <- lapply(label, function(l) {
      id <- which(x$labels == label)
      rind <- which(x$atlas == id)
      neuroim2::ROIVol(space(x$atlas),coords = index_to_grid(x$atlas, rind),data=x$ids[id])
    })
    names(ret) <- label
    ret
  } else {
    ret <- lapply(id, function(i) {
      rind <- which(x$atlas == i)
      neuroim2::ROIVol(space(x$atlas),coords = index_to_grid(x$atlas, rind),data=i)
    })
    names(ret) <- id
    ret
  }
}
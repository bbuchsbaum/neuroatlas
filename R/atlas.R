
#' @export
print.atlas <- function(x) {
  cat("Atlas:", x$name, "\n")
  cat("num regions: ", length(x$ids), "\n")
}
  

create_cache_dir <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  if (!dir.exists(dname)) {
    dir.create(dname)
  }
  dname
}

get_cache_dir <- function() {
  create_cache_dir()
  #paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
}

clear_cache <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  fnames <- list.files(dname, full.names=TRUE)
  sapply(fnames, unlink)
}


#' @export
merge_atlases <- function(atlas1, atlas2) {
  assertthat::assert_that(all(dim(atlas1$atas) == dim(atlas2$atlas)))

  atl2 <- atlas2$atlas
  atl2[atl2 != 0] <- atl2[atl2 != 0] + max(atlas1$ids) + 1
  atlmerged <- atlas1$atlas
  atlmerged[atl2 != 0] <- atl2[atl2 != 0]

  ret <- list(
    name=paste0(atlas1$name,"::", atlas2$name),
    atlas=atlmerged,
    cmap=rbind(atlas1$cmap, atlas2$cmap),
    ids=c(atlas1$ids, atlas2$ids),
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



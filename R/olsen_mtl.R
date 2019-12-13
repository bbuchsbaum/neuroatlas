#' Medial Temporal Lobe atlas from Rosanna Olsen
#'
#' @docType data
#' @name olsen_mtl
#' @keywords datasets
#'
#' @format A list of the 'atlas' class with 7 elements
#' \describe{
#'   \item{name}{the name of the atlas}
#'   \item{atlas}{the atlas in MNI space with 1X1X1 resolution }
#'   \item{labels}{the atlas labels}
#'   \item{orig_labels}{the full labels (including hemisphere)}
#'   \item{ids}{the integer ids from 1 to 16}
#'   \item{hemi}{the hemisphere of each region}
#' }
#'
#' @examples
#' data(olsen_mtl)
"olsen_mtl"


get_olsen_mtl <- function(outspace=NULL) {
  if (is.null(outspace)) {
    data(olsen_mtl)
    olsen_mtl
  } else {
    atres <- resample(olsen_mtl$atlas, outspace)
    tmp <- olsen_mtl
    tmp$atlas <- atres
    tmp
  }

}

#' @export
get_hipp_atlas <- function(outspace=NULL, apsections=1) {
  x <- if (is.null(outspace)) {
    data(olsen_mtl)
    olsen_mtl
  } else {
    atres <- resample(olsen_mtl$atlas, outspace)
    tmp <- olsen_mtl
    tmp$atlas <- atres
    tmp
  }

  atlas <- x$atlas
  atlas[atlas %in% c(1,2,3,6,8,9,10,11,14,16)] <- 1
  atlas[!(atlas %in% c(1,2,3,6,8,9,10,11,14,16))] <- 0

  ind <- which(atlas>0)
  grid <- neuroim2::index_to_coord(atlas, which(atlas > 0))
  if (apsections > 1) {
    qz <- cut(grid[,3], apsections)
    levels(qz) <- paste0(seq(1,apsections))
    for (lev in levels(qz)) {
      atlas[ind[qz == lev]] <- as.numeric(lev)
    }
    atlas[ind[grid[,1] > 0]] <- atlas[ind[grid[,1] > 0]] + apsections
  } else {
    atlas[ind[grid[,1] > 0]] <- 2
  }

  ret <- list(
      name="hippocampus",
      atlas=atlas,
      ids=seq(1,apsections*2),
      labels=c(paste0("hippocampus_", seq(1,apsections)),
               paste0("hippocampus_", seq(1,apsections))),
      hemi=c(rep("left", apsections), rep("right", apsections)),
      cmap=t(col2rgb(rainbow(apsections*2)))
    )

  ret$orig_labels <- paste0(ret$hemi, "_", ret$labels)
  class(ret) <- c("hippocampus", "atlas")
  ret

}


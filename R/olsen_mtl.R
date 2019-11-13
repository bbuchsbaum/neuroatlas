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

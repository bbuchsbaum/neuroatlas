
get_roi <- function(x, label, id, hemi) {
  UseMethod("get_roi")
}


#' map a set of values to an atlas
#' 
#' @param x the atlas to map values to
#' @param vals the values to map
#' @param thresh optional (min,max) threshold vector
#' @export
map_atlas <- function(x, vals, thresh, ...) {
  UseMethod("map_atlas")
}

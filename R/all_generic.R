#' Extract a region of interest (ROI) from an atlas
#' 
#' @description
#' Extracts a specific region of interest from an atlas object based on label, ID,
#' and hemisphere information.
#'
#' @param x An atlas object
#' @param label Character string specifying the ROI label/name
#' @param id Numeric ID of the ROI in the atlas
#' @param hemi Character string specifying hemisphere ('left' or 'right')
#'
#' @return Returns a subset of the atlas containing only the specified ROI
#'
#' @examples
#' \dontrun{
#' # Load the aseg atlas
#' atlas <- get_aseg_atlas()
#'
#' # Extract the hippocampus ROI
#' roi <- get_roi(atlas, label = "Hippocampus")
#' }
#' @export
get_roi <- function(x, label, id, hemi) {
  UseMethod("get_roi")
}

#' Map values to an atlas
#' 
#' @description
#' Maps a set of values to regions/parcels in an atlas object. This can be used
#' to visualize data (like statistics or measurements) across atlas regions.
#'
#' @param x An atlas object to map values onto
#' @param vals Numeric vector of values to map to atlas regions. Length should match
#'   the number of regions in the atlas
#' @param thresh Optional numeric vector of length 2 specifying (min, max) thresholds
#'   for the mapped values. Values outside this range will be clamped.
#' @param ... Additional arguments passed to methods
#'
#' @return Returns the atlas object with mapped values
#'
#' @examples
#' \dontrun{
#' # Load the aseg atlas
#' atlas <- get_aseg_atlas()
#' vals <- rnorm(length(atlas$orig_labels))
#'
#' # Map values with a threshold of -2 to 2
#' mapped <- map_atlas(atlas, vals, thresh = c(-2, 2))
#' }
#' @export
map_atlas <- function(x, vals, thresh, ...) {
  UseMethod("map_atlas")
}

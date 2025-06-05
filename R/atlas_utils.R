#' Internal Helper: Get Atlas Volume
#'
#' Retrieves the volume component from an atlas object.
#'
#' @param atlas An object of class 'atlas' or similar containing a volume
#'   definition in either `atlas$atlas` or `atlas$data`.
#'
#' @return A `NeuroVol` object used to define ROIs.
#' @keywords internal
#' @noRd
.get_atlas_volume <- function(atlas) {
  vol <- NULL

  if (!is.null(atlas$atlas) && methods::is(atlas$atlas, "NeuroVol")) {
    vol <- atlas$atlas
  } else if (!is.null(atlas$atlas) && methods::is(atlas$atlas, "ClusteredNeuroVol")) {
    vol <- atlas$atlas
  } else if (!is.null(atlas$data) && methods::is(atlas$data, "NeuroVol")) {
    vol <- atlas$data
  } else if (!is.null(atlas$data) && methods::is(atlas$data, "ClusteredNeuroVol")) {
    vol <- atlas$data
  }

  if (is.null(vol)) {
    stop("Could not determine atlas volume from object")
  }
  vol
}

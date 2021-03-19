
#' @export
get_template <- function(name="MNI152NLin6Asym", desc="brain", resolution=1, suffix="T1w", extension=".nii.gz") {
  ret <- tflow$api$get(name, resolution=resolution, desc=desc, suffix=suffix, extension=extension)
  if (length(ret) > 1) {
    warning(paste("matched retrieved more than one file, returning first: ", ret[[1]]))
    neuroim2::read_vol(ret[[1]]$as_posix())
  } else if (length(ret) == 1) {
    neuroim2::read_vol(ret$as_posix())
  }
}

#' @export
templates <- function() {
  tflow$api$templates()
}
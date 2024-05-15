
#' @export
get_template <- function(name="MNI152NLin2009cAsym", desc="brain", resolution=1, label=NULL, atlas=NULL, suffix="T1w", extension=".nii.gz") {
  ret <- tflow$api$get(name, resolution=resolution, desc=desc, label=label, suffix=suffix, extension=extension)
  #if (length(ret) > 1) {
  #  warning(paste("matched retrieved more than one file, returning first: ", ret[[1]]))
  #  neuroim2::read_vol(ret[[1]]$as_posix())
  #} else if (length(ret) == 1) {
  neuroim2::read_vol(ret$as_posix())
  #}
}

get_template_brainmask <- function(name="MNI152NLin2009cAsym", resolution=1, extension=".nii.gz") {
  get_template(name=name, desc="brainmask", suffix="mask", resolution=resolution)
}

get_template_probseg <- function(name="MNI152NLin2009cAsym", label="GM", resolution=1,extension=".nii.gz") {
  get_template(name=name, desc=NULL, label=label, suffix="probseg", resolution=resolution)
}

get_template_schaefer <- function(name="MNI152NLin2009cAsym", resolution=1,parcels=400, networks=17, extension=".nii.gz") {
  desc <- paste0(parcels, "Parcels", networks, "Networks")
  get_template(name=name, desc=desc, atlas="Schaefer2018", suffix="dseg", resolution=resolution)
}

#' @export
templates <- function() {
  tflow$api$templates()
}
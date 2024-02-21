#' Get the ASEG Atlas
#'
#' This function reads the ASEG atlas file provided by the neuroatlas package and
#' returns a list containing the atlas volume, colormap, region IDs, labels, and hemisphere information.
#' The atlas can be resampled to a different space if the `outspace` argument is provided.
#'
#' @param outspace (Optional) A NeuroSpace object specifying the desired output space for the atlas.
#'                 If provided, the atlas will be resampled to this space. Default is `NULL`,
#'                 meaning the atlas will be returned in its original space.
#'
#' @return A list with class 'aseg' and 'atlas' containing the following elements:
#' \itemize{
#'   \item{atlas}{A NeuroVol object representing the atlas volume.}
#'   \item{cmap}{A data frame with columns 'red', 'green', and 'blue', representing the colormap for the atlas.}
#'   \item{ids}{A numeric vector containing the unique region IDs in the atlas.}
#'   \item{labels}{A character vector containing the anatomical region labels corresponding to the region IDs.}
#'   \item{hemi}{A character vector containing the hemisphere information ('left', 'right', or 'NA') for each region.}
#' }
#'
#' @export
get_aseg_atlas <- function(outspace=NULL) {
  fname <- system.file("extdata/atlas_aparc_aseg_prob33.nii.gz", package="neuroatlas")
  atlas <- neuroim2::read_vol(fname)

  if (!is.null(outspace)) {
    atlas <- resample(atlas, outspace)
  }

  ids <- sort(unique(as.vector(atlas))[-1])
  labels <- c(
    "Thalamus",
    "Caudate",
    "Putamen",
    "Pallidum",
    "Brainstem",
    "Hippocampus",
    "Amygdala",
    "Accumbens",
    "VentralDC",
    "Thalamus",
    "Caudate",
    "Putamen",
    "Pallidum",
    "Hippocampus",
    "Amygdala",
    "Accumbens",
    "VentralDC")

  hemi=c(rep("left", 4), NA, rep("left", 3), rep("right", 8))
  cmap <- tibble::tribble(
    ~red, ~green, ~blue,
    0,   118, 14,
    122, 186, 220,
    236, 13,  176,
    12,  48,  255,
    119, 159, 176,
    220, 216, 20,
    220, 216, 20,
    255, 165, 0,
    165, 42,  42,
    0,   118, 14,
    122, 186, 220,
    236, 13,  176,
    13,  48,  255,
    220, 216, 20,
    103, 255, 255,
    255, 165, 0,
    165, 42,  42)

  ret <- list(
    atlas=atlas,
    cmap=cmap,
    ids=ids,
    labels=labels,
    hemi=hemi)

  class(ret) <- c("aseg", "atlas")
  ret
}

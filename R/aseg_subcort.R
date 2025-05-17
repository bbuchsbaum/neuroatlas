#' Get the FreeSurfer Subcortical Atlas (ASEG)
#'
#' @description
#' Loads and returns the FreeSurfer subcortical segmentation (ASEG) atlas, which provides
#' probabilistic labels for key subcortical structures in the brain. The atlas includes
#' bilateral structures such as the thalamus, caudate, putamen, and limbic regions,
#' as well as midline structures like the brainstem.
#'
#' @details
#' The ASEG atlas is derived from FreeSurfer's automatic subcortical segmentation
#' algorithm and has been transformed into standard space. Each voxel contains an
#' integer ID corresponding to a specific anatomical structure. The atlas includes
#' major subcortical structures for both hemispheres:
#' \itemize{
#'   \item Bilateral deep gray structures (thalamus, caudate, putamen, pallidum)
#'   \item Limbic structures (hippocampus, amygdala)
#'   \item Ventral structures (nucleus accumbens, ventral diencephalon)
#'   \item Midline structures (brainstem)
#' }
#'
#' @param outspace Optional \code{NeuroSpace} object specifying the desired output space
#'   for resampling the atlas. If NULL (default), returns the atlas in its native space.
#'
#' @return A list with classes 'aseg' and 'atlas' containing:
#' \describe{
#'   \item{atlas}{A \code{NeuroVol} object containing the 3D volume of atlas labels}
#'   \item{cmap}{A data frame with RGB color specifications for each region}
#'   \item{ids}{Integer vector of region IDs present in the atlas}
#'   \item{labels}{Character vector of anatomical labels corresponding to each ID}
#'   \item{hemi}{Character vector indicating hemisphere ('left', 'right', or NA) for each region}
#' }
#'
#' @examples
#' \dontrun{
#' # Load the atlas in native space
#' aseg <- get_aseg_atlas()
#'
#' # View the available region labels
#' aseg$labels
#'
#' # Get the unique region IDs
#' aseg$ids
#' }
#'
#' @references
#' Fischl, B., et al. (2002). Whole brain segmentation: automated labeling of
#' neuroanatomical structures in the human brain. Neuron, 33(3), 341-355.
#'
#' @seealso
#' \code{\link{map_atlas}} for mapping values onto atlas regions
#' \code{\link{get_roi}} for extracting specific regions of interest
#'
#' @importFrom neuroim2 read_vol
#' @importFrom tibble tribble
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
    name="ASEG",
    atlas=atlas,
    cmap=cmap,
    ids=ids,
    labels=labels,
    orig_labels=labels,
    hemi=hemi,
    network=NULL)

  class(ret) <- c("aseg", "atlas")
  ret
}

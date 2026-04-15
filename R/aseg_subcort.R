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
  template_space <- .template_space_from_outspace(
    outspace,
    default_space = "MNI152NLin6Asym"
  )

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

  hemi=c(rep("left", 4), NA, rep("left", 3), NA, rep("right", 8))
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

  # Build roi_metadata tibble
  ret$roi_metadata <- tibble::tibble(
    id = ret$ids,
    label = ret$labels,
    label_full = ret$orig_labels,
    hemi = ret$hemi,
    color_r = as.integer(cmap$red),
    color_g = as.integer(cmap$green),
    color_b = as.integer(cmap$blue)
  )

  class(ret) <- c("aseg", "atlas")
  ref <- new_atlas_ref(
    family = "aseg",
    model = "FreeSurferASEG",
    representation = "volume",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = "1mm",
    provenance = "inst/extdata/atlas_aparc_aseg_prob33.nii.gz",
    source = "bundled_extdata",
    lineage = "Bundled package atlas volume.",
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = paste(
      "Header (193x229x193; 1mm; RAS) matches MNI152NLin6Asym.",
      "FreeSurfer aparc+aseg uses FSL's MNI152 as standard space.",
      "Confirmed via data-raw/audit_bundled_spaces.R."
    )
  )

  artifacts <- .new_atlas_artifact(
    role = "parcellation_volume",
    family = "aseg",
    model = "FreeSurferASEG",
    source_name = "neuroatlas",
    source_url = "inst/extdata/atlas_aparc_aseg_prob33.nii.gz",
    source_ref = "atlas_aparc_aseg_prob33.nii.gz",
    citation_doi = "10.1016/S0896-6273(02)00569-X",
    file_name = "atlas_aparc_aseg_prob33.nii.gz",
    template_space = "MNI152NLin6Asym",
    coord_space = "MNI152",
    resolution = "1mm",
    lineage = "Bundled neuroatlas volume derived from FreeSurfer ASEG labels.",
    confidence = "high",
    notes = "Packaged in inst/extdata."
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = "MNI152NLin6Asym",
    to_template_space = "MNI152NLin6Asym",
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = "high",
    details = "Loaded bundled ASEG atlas."
  )
  if (!is.null(outspace)) {
    history <- dplyr::bind_rows(
      history,
      .new_atlas_history(
        action = "resample",
        representation = "volume",
        from_template_space = "MNI152NLin6Asym",
        to_template_space = template_space,
        from_coord_space = "MNI152",
        to_coord_space = "MNI152",
        status = "available",
        confidence = "approximate",
        details = "Resampled ASEG atlas to requested output space."
      )
    )
  }

  ret <- .attach_atlas_ref(ret, ref)
  ret <- .attach_atlas_provenance(ret, artifacts = artifacts, history = history)
  ret
}

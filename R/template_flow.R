#' Access Templateflow Brain Templates
#'
#' @description
#' Retrieves standardized neuroimaging templates from the Templateflow repository.
#' Provides access to various brain templates, masks, segmentations, and atlases
#' in standardized spaces.
#'
#' @details
#' The function provides access to various template resources:
#' 
#' Template Spaces Available:
#' \itemize{
#'   \item MNI152NLin2009cAsym: Non-linear asymmetric MNI template (default)
#'   \item MNI152NLin6Asym: Older non-linear MNI template
#'   \item OASIS30ANTs: Template derived from OASIS dataset
#'   \item NKI: Nathan Kline Institute template
#'   \item WHS: Waxholm space rat template
#' }
#'
#' Common Descriptors (desc):
#' \itemize{
#'   \item brain: Brain-extracted template
#'   \item head: Full head template
#'   \item mask: Binary brain mask
#'   \item dseg: Discrete segmentation
#'   \item probseg: Probability maps
#' }
#'
#' Labels for Segmentations:
#' \itemize{
#'   \item GM: Gray matter
#'   \item WM: White matter
#'   \item CSF: Cerebrospinal fluid
#' }
#'
#' @param name Character string specifying template name. Default: "MNI152NLin2009cAsym"
#' @param desc Character string describing template variant. Default: "brain"
#' @param resolution Numeric resolution in mm. Default: 1
#' @param label Character string specifying tissue label for probability maps
#' @param atlas Character string specifying atlas name
#' @param suffix Character string specifying image type. Default: "T1w"
#' @param extension Character string specifying file extension. Default: ".nii.gz"
#'
#' @return A NeuroVol object containing the requested template
#'
#' @examples
#' \dontrun{
#' # Get standard brain-extracted MNI template
#' mni <- get_template()
#'
#' # Get 2mm resolution template
#' mni_2mm <- get_template(resolution = 2)
#'
#' # Get gray matter probability map
#' gm_prob <- get_template_probseg(label = "GM")
#'
#' # Get brain mask
#' mask <- get_template_brainmask()
#' }
#'
#' @seealso
#' \code{\link{get_template_brainmask}}, \code{\link{get_template_probseg}},
#' \code{\link{get_template_schaefer}}
#'
#' @source
#' \url{https://www.templateflow.org/}
#'
#' @references
#' Ciric R et al., (2021) TemplateFlow: a community archive of imaging templates
#' and atlases for improved consistency in neuroimaging. Nature Methods, 18, 1276-1278.
#'
#' @importFrom neuroim2 read_vol
#' @export
get_template <- function(name="MNI152NLin2009cAsym", desc="brain", resolution=1, 
                        label=NULL, atlas=NULL, suffix="T1w", 
                        extension=".nii.gz") {
  ret <- tflow$api$get(name, resolution=resolution, desc=desc, label=label, 
                       suffix=suffix, extension=extension)
  neuroim2::read_vol(ret$as_posix())
}

#' Get Brain Mask from Template
#'
#' @description
#' Convenience function to retrieve a binary brain mask for a specified template.
#'
#' @inheritParams get_template
#' @return A NeuroVol object containing the binary brain mask
#' @export
get_template_brainmask <- function(name="MNI152NLin2009cAsym", resolution=1, 
                                  extension=".nii.gz") {
  get_template(name=name, desc="brainmask", suffix="mask", 
              resolution=resolution)
}

#' Get Tissue Probability Map from Template
#'
#' @description
#' Retrieves probability maps for different tissue types (GM, WM, CSF).
#'
#' @inheritParams get_template
#' @param label Character string specifying tissue type ("GM", "WM", or "CSF")
#' @return A NeuroVol object containing the probability map
#' @export
get_template_probseg <- function(name="MNI152NLin2009cAsym", label="GM", 
                                resolution=1, extension=".nii.gz") {
  get_template(name=name, desc=NULL, label=label, suffix="probseg", 
              resolution=resolution)
}

#' Get Schaefer Parcellation in Template Space
#'
#' @description
#' Retrieves Schaefer cortical parcellation mapped to a specified template space.
#'
#' @inheritParams get_template
#' @param parcels Number of parcels (400 default)
#' @param networks Number of networks (17 default)
#' @return A NeuroVol object containing the parcellation
#' @export
get_template_schaefer <- function(name="MNI152NLin2009cAsym", resolution=1,
                                 parcels=400, networks=17, extension=".nii.gz") {
  desc <- paste0(parcels, "Parcels", networks, "Networks")
  get_template(name=name, desc=desc, atlas="Schaefer2018", suffix="dseg", 
              resolution=resolution)
}

#' List Available Templates
#'
#' @description
#' Returns a list of all available templates in the Templateflow repository.
#'
#' @return A character vector of available template names
#' @export
templates <- function() {
  tflow$api$templates()
}

#' Get Template Head Image
#'
#' @description
#' Convenience function to get the full head (non-brain-extracted) template.
#'
#' @inheritParams get_template
#' @return A NeuroVol object containing the head template
#' @export
get_template_head <- function(name="MNI152NLin2009cAsym", resolution=1, 
                            extension=".nii.gz") {
  get_template(name=name, desc="head", suffix="T1w", resolution=resolution)
}

#' Get CSF Probability Map
#'
#' @description
#' Convenience function to get CSF probability map.
#'
#' @inheritParams get_template
#' @return A NeuroVol object containing the CSF probability map
#' @export
get_template_csf <- function(name="MNI152NLin2009cAsym", resolution=1, 
                            extension=".nii.gz") {
  get_template_probseg(name=name, label="CSF", resolution=resolution)
}

#' Get Gray Matter Probability Map
#'
#' @description
#' Convenience function to get gray matter probability map.
#'
#' @inheritParams get_template
#' @return A NeuroVol object containing the gray matter probability map
#' @export
get_template_gm <- function(name="MNI152NLin2009cAsym", resolution=1, 
                           extension=".nii.gz") {
  get_template_probseg(name=name, label="GM", resolution=resolution)
}

#' Get White Matter Probability Map
#'
#' @description
#' Convenience function to get white matter probability map.
#'
#' @inheritParams get_template
#' @return A NeuroVol object containing the white matter probability map
#' @export
get_template_wm <- function(name="MNI152NLin2009cAsym", resolution=1, 
                           extension=".nii.gz") {
  get_template_probseg(name=name, label="WM", resolution=resolution)
}
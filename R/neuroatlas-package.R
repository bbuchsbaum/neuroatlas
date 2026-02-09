#' @keywords internal
"_PACKAGE"

#' neuroatlas: Neuroimaging Atlases and Parcellations
#'
#' The neuroatlas package provides a unified interface to access and work with
#' various neuroimaging atlases and parcellations. It includes support for
#' cortical atlases (Schaefer, Glasser), subcortical segmentations (FreeSurfer ASEG),
#' and specialized atlases (Olsen MTL). The package integrates with TemplateFlow
#' for standardized template access and supports interactive brain surface
#' visualisation via triangle-mesh rendering.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{get_schaefer_atlas}}}{Access Schaefer cortical parcellations}
#'   \item{\code{\link{get_glasser_atlas}}}{Access Glasser multi-modal parcellation}
#'   \item{\code{\link{get_aseg_atlas}}}{Access FreeSurfer subcortical segmentation}
#'   \item{\code{\link{get_olsen_mtl}}}{Access Olsen medial temporal lobe atlas}
#'   \item{\code{\link{get_template}}}{Fetch templates from TemplateFlow}
#' }
#'
#' @section Atlas Operations:
#' \describe{
#'   \item{\code{\link{get_roi}}}{Extract specific regions from an atlas}
#'   \item{\code{\link{map_atlas}}}{Map values to atlas regions}
#'   \item{\code{\link{reduce_atlas}}}{Combine regions within an atlas}
#'   \item{\code{\link{merge_atlases}}}{Combine multiple atlases}
#'   \item{\code{\link{dilate_atlas}}}{Expand atlas regions into unassigned voxels}
#' }
#'
#' @section Visualization:
#' \describe{
#'   \item{\code{\link{plot_brain}}}{Interactive cortical surface rendering}
#'   \item{\code{\link{plot.atlas}}}{Plot volumetric atlas objects}
#'   \item{\code{\link{atlas_roi_colors}}}{Optimal ROI colour assignment}
#' }
#'
#' @section TemplateFlow Integration:
#' Access standardized neuroimaging templates:
#' \describe{
#'   \item{\code{\link{create_templateflow}}}{Initialize TemplateFlow connection}
#'   \item{\code{\link{tflow_spaces}}}{List available template spaces}
#'   \item{\code{\link{install_templateflow}}}{Install Python TemplateFlow module}
#' }
#'
#' @name neuroatlas-package
#' @aliases neuroatlas
NULL

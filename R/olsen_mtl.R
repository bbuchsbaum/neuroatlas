#' Olsen Medial Temporal Lobe Atlas
#'
#' @description
#' A detailed parcellation atlas of the medial temporal lobe (MTL) regions,
#' including hippocampus and surrounding cortical areas, based on the work of
#' Rosanna Olsen and colleagues.
#'
#' @details
#' The atlas provides a detailed segmentation of MTL structures in MNI space at 1mm
#' resolution. It includes bilateral parcellation of:
#' \itemize{
#'   \item Hippocampal subfields
#'   \item Perirhinal cortex
#'   \item Entorhinal cortex
#'   \item Parahippocampal cortex
#' }
#'
#' @format A list with class 'atlas' containing:
#' \describe{
#'   \item{name}{Character string identifying the atlas}
#'   \item{atlas}{NeuroVol object containing the parcellation in 1mm MNI space}
#'   \item{labels}{Character vector of anatomical region labels}
#'   \item{orig_labels}{Full region labels including hemisphere information}
#'   \item{ids}{Integer vector of region IDs (1:16)}
#'   \item{hemi}{Character vector indicating hemisphere ('left' or 'right')}
#' }
#'
#' @source
#' Olsen, R. K., et al. (2013). The role of relational binding in item memory:
#' Evidence from face recognition in a case of developmental amnesia.
#' Journal of Neuroscience, 33(36), 14107-14111.
#'
#' @examples
#' \dontrun{
#' # Load the atlas data
#' data(olsen_mtl)
#'
#' # View available regions
#' olsen_mtl$labels
#'
#' # Check distribution across hemispheres
#' table(olsen_mtl$hemi)
#' }
#'
#' @keywords datasets
"olsen_mtl"

#' Load Olsen MTL Atlas
#'
#' @description
#' Loads the Olsen medial temporal lobe atlas and optionally resamples it to a
#' different space.
#'
#' @param outspace Optional \code{NeuroSpace} object specifying desired output space.
#'   If NULL (default), returns atlas in native 1mm MNI space.
#'
#' @return A list with class 'atlas' containing the MTL parcellation
#'
#' @examples
#' \dontrun{
#' # Load in native space
#' mtl <- get_olsen_mtl()
#'
#' # Load and resample to a different space
#' mtl_resampled <- get_olsen_mtl(outspace = my_space)
#' }
#'
#' @seealso
#' \code{\link{get_hipp_atlas}} for hippocampus-specific parcellation
#'
#' @export
get_olsen_mtl <- function(outspace=NULL) {
  data(olsen_mtl)
  if (is.null(outspace)) {
    olsen_mtl
  } else {
    atres <- resample(olsen_mtl$atlas, outspace)
    tmp <- olsen_mtl
    tmp$atlas <- atres
    tmp
  }
}

#' Extract Hippocampal Parcellation
#'
#' @description
#' Creates a hippocampus-specific atlas from the Olsen MTL atlas, with optional
#' anterior-posterior subdivisions.
#'
#' @details
#' This function extracts hippocampal regions from the full MTL atlas and can
#' subdivide them into anterior-posterior segments. The resulting atlas maintains
#' bilateral organization and can be used for targeted hippocampal analyses.
#'
#' @param outspace Optional \code{NeuroSpace} object for resampling
#' @param apsections Integer specifying number of anterior-posterior divisions.
#'   Default: 1 (no subdivision)
#'
#' @return A list with class c("hippocampus", "atlas") containing:
#' \describe{
#'   \item{name}{Character string "hippocampus"}
#'   \item{atlas}{NeuroVol object with hippocampal parcellation}
#'   \item{ids}{Integer vector of region IDs}
#'   \item{labels}{Character vector of region labels}
#'   \item{hemi}{Character vector of hemisphere designations}
#'   \item{cmap}{Matrix of RGB colors for visualization}
#'   \item{orig_labels}{Full labels including hemisphere information}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic hippocampal atlas
#' hipp <- get_hipp_atlas()
#'
#' # With anterior-posterior subdivisions
#' hipp_ap <- get_hipp_atlas(apsections = 3)
#' }
#'
#' @importFrom neuroim2 index_to_coord
#' @export
get_hipp_atlas <- function(outspace=NULL, apsections=1) {
  # Load and potentially resample base atlas
  x <- if (is.null(outspace)) {
    data(olsen_mtl)
    olsen_mtl
  } else {
    data(olsen_mtl)
    atres <- resample(olsen_mtl$atlas, outspace)
    tmp <- olsen_mtl
    tmp$atlas <- atres
    tmp
  }

  # Extract hippocampal regions
  atlas <- x$atlas
  atlas[atlas %in% c(1,2,3,6,8,9,10,11,14,16)] <- 1
  atlas[!(atlas %in% c(1,2,3,6,8,9,10,11,14,16))] <- 0

  ind <- which(atlas > 0)
  grid <- neuroim2::index_to_coord(atlas, which(atlas > 0))

  # Create anterior-posterior subdivisions if requested
  if (apsections > 1) {
    qz <- cut(grid[,2], apsections)
    levels(qz) <- paste0(seq(1,apsections))
    for (lev in levels(qz)) {
      atlas[ind[qz == lev]] <- as.numeric(lev)
    }
    atlas[ind[grid[,1] > 0]] <- atlas[ind[grid[,1] > 0]] + apsections
  } else {
    atlas[ind[grid[,1] > 0]] <- 2
  }

  # Create return object
  ret <- list(
    name = "hippocampus",
    atlas = atlas,
    ids = seq(1, apsections*2),
    labels = c(paste0("hippocampus_", seq(1,apsections)),
              paste0("hippocampus_", seq(1,apsections))),
    hemi = c(rep("left", apsections), rep("right", apsections)),
    cmap = t(col2rgb(rainbow(apsections*2)))
  )

  ret$orig_labels <- paste0(ret$hemi, "_", ret$labels)
  class(ret) <- c("hippocampus", "atlas")
  ret
}


#' Schaefer Atlas with 17 Networks and 200 Parcels
#'
#' @description
#' Pre-loaded Schaefer cortical parcellation with 200 regions organized into 17 
#' functional networks. This atlas divides the cerebral cortex based on resting-state 
#' functional connectivity patterns.
#'
#' @format A list with class 'schaefer' and 'atlas' containing:
#' \describe{
#'   \item{name}{Character string "schaefer17_200"}
#'   \item{atlas}{A \code{ClusteredNeuroVol} object with the parcellation}
#'   \item{cmap}{Data frame with RGB color values for visualization}
#'   \item{ids}{Integer vector of region IDs (1:200)}
#'   \item{labels}{Character vector of region labels}
#'   \item{orig_labels}{Original Schaefer labels}
#'   \item{network}{Network assignment for each region}
#'   \item{hemi}{Hemisphere designation ('left' or 'right')}
#' }
#'
#' @source
#' \url{https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal}
#'
#' @references
#' Schaefer, A., et al. (2018). Local-Global Parcellation of the Human Cerebral
#' Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9),
#' 3095-3114.
#'
#' @examples
#' \donttest{
#' data(Schaefer17_200)
#' print(Schaefer17_200)
#' table(Schaefer17_200$network)
#' }
#'
#' @keywords datasets
"Schaefer17_200"

#' Schaefer Atlas with 17 Networks and 400 Parcels
#'
#' @description
#' Pre-loaded Schaefer cortical parcellation with 400 regions organized into 17 
#' functional networks. This atlas divides the cerebral cortex based on resting-state 
#' functional connectivity patterns.
#'
#' @format A list with class 'schaefer' and 'atlas' containing:
#' \describe{
#'   \item{name}{Character string "schaefer17_400"}
#'   \item{atlas}{A \code{ClusteredNeuroVol} object with the parcellation}
#'   \item{cmap}{Data frame with RGB color values for visualization}
#'   \item{ids}{Integer vector of region IDs (1:400)}
#'   \item{labels}{Character vector of region labels}
#'   \item{orig_labels}{Original Schaefer labels}
#'   \item{network}{Network assignment for each region}
#'   \item{hemi}{Hemisphere designation ('left' or 'right')}
#' }
#'
#' @source
#' \url{https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal}
#'
#' @references
#' Schaefer, A., et al. (2018). Local-Global Parcellation of the Human Cerebral
#' Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9),
#' 3095-3114.
#'
#' @examples
#' \donttest{
#' data(Schaefer17_400)
#' print(Schaefer17_400)
#' table(Schaefer17_400$network)
#' }
#'
#' @keywords datasets
"Schaefer17_400"

#' Schaefer Atlas with 17 Networks and 600 Parcels
#'
#' @description
#' Pre-loaded Schaefer cortical parcellation with 600 regions organized into 17 
#' functional networks. This atlas divides the cerebral cortex based on resting-state 
#' functional connectivity patterns.
#'
#' @format A list with class 'schaefer' and 'atlas' containing:
#' \describe{
#'   \item{name}{Character string "schaefer17_600"}
#'   \item{atlas}{A \code{ClusteredNeuroVol} object with the parcellation}
#'   \item{cmap}{Data frame with RGB color values for visualization}
#'   \item{ids}{Integer vector of region IDs (1:600)}
#'   \item{labels}{Character vector of region labels}
#'   \item{orig_labels}{Original Schaefer labels}
#'   \item{network}{Network assignment for each region}
#'   \item{hemi}{Hemisphere designation ('left' or 'right')}
#' }
#'
#' @source
#' \url{https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal}
#'
#' @references
#' Schaefer, A., et al. (2018). Local-Global Parcellation of the Human Cerebral
#' Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9),
#' 3095-3114.
#'
#' @examples
#' \donttest{
#' data(Schaefer17_600)
#' print(Schaefer17_600)
#' table(Schaefer17_600$network)
#' }
#'
#' @keywords datasets
"Schaefer17_600"
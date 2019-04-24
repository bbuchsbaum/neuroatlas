

#' Retrieve and load Schaefer network parcellation from github repository
#'
#' @param parcels the number rof parcels in hte atlas
#' @param networks the network number
#' @param resolution the resolution of the atlas in MNI space (1mm or 2mm)
#' @param outdim an optional 3-element vector indicating dimensions to resample atlas to.
#' @return a an image of type \code{NeuroVol}
#'
#' @examples
#'
#' v1 <- get_schaefer_atlas(parcels="300")
#' v2 <- get_schaefer_atlas(parcels="300", outdim=c(40,40,40))
#'
#' @details
#'
#' Files are downloaded from the github repository: https://github.com/ThomasYeoLab/CBIG/
get_schaefer_atlas <- function(parcels=c("100","200","300","400","500","600","800","1000"),
                               networks=c("7","17"),resolution=c("1","2"), outdim=NULL) {

  parcels <- match.arg(parcels)
  networks <- match.arg(networks)
  resolution <- match.arg(resolution)

  fname <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order_FSLMNI152_", resolution, "mm.nii.gz")
  path <- paste0("https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/",
                 fname)

  des <- paste0(tempdir(), "/", fname)
  ret <- download(path, des)

  vol <- read_vol(des)

  if (!is.null(outdim)) {
    assertthat::assert_that(length(outdim) == 3)
    arr <- vol@.Data
    ospacing <- dim(vol)/outdim * spacing(vol)
    im <- imager::as.cimg(arr)
    imr <- resize(im, outdim[1], outdim[2], outdim[3],interpolation_type=1)
    arr2 <- drop(as.array(imr))
    vol <- NeuroVol(arr2, NeuroSpace(outdim, ospacing))
  }

  vol

}



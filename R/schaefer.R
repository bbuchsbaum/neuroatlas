

#' Retrieve and load Schaefer network parcellation from github repository
#'
#' @param parcels the number rof parcels in hte atlas
#' @param networks the network number
#' @param resolution the resolution of the atlas in MNI space (1mm or 2mm)
#' @param outdim an optional 3-element vector indicating dimensions to resample atlas to.
#' @export
#'
#' @importFrom neuroim2 read_vol
#' @importFrom downloader download
#'
#' @examples
#'
#' v1 <- get_schaefer_atlas(parcels="300")
#' v2 <- get_schaefer_atlas(parcels="300", outdim=c(40,40,40))
#'
#' @return
#'
#' a list with three elemenents:
#'
#' \describe{
#'   \item{atlas}{ a NeuroVol instance with integer indices coding the ROIs}
#'   \item{cmap}{ a data.frame indicating the colors associated with each ROI}
#'   \item{legend}{a data.frame providing label information about each ROI}
#' }
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

  rpath <- "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"
  path <- paste0(rpath,fname)

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

  label_name <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order.txt")
  des2 <- paste0(tempdir(), "/", label_name)
  ret <- download(paste0(rpath, label_name), des2)
  labels <- read.table(des2, as.is=TRUE)
  labels <- labels[, 1:5]
  names(labels) <- c("ROINUM", "label", "red", "green", "blue")
  labels$label <- gsub(paste0(networks, "Networks", "_"), "", labels$label)
  hemi <- substr(labels$label, 1,2)
  labels$hemi <- hemi

  labels$hemi <- sapply(strsplit(labels$label, "_"), "[[", 1)
  labels$network <-  sapply(strsplit(labels$label, "_"), "[[", 2)
  labels$name <-   sapply(strsplit(labels$label, "_"), function(x) paste(x[(length(x)-1):length(x)], collapse="_"))

  ret <- list(
    atlas=vol,
    cmap=labels[,3:5],
    ids=1:nrow(labels),
    labels=labels$name,
    network=labels$network,
    hemi=labels$hemi)

  class(ret) <- "atlas"
  ret
}



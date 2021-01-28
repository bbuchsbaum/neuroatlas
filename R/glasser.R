#' Retrieve and load Glasser360 network parcellation from PennBBL github repository
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
#' v1 <- get_glasser_atlas()
#' 
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
#' Files are downloaded from the github repository: https://github.com/PennBBL/xcpEngine/
get_glasser_atlas <- function(outspace=NULL) {
  
  fname <- "glasser360MNI.nii.gz"
  rpath= "https://github.com/PennBBL/xcpEngine/raw/master/atlas/glasser360/"
  path <- paste0(rpath,fname)
  
  des <- paste0(tempdir(), "/", fname)
  ret <- download(path, des)
  
  vol <- neuroim2::read_vol(des)
  
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace)
  }
  
  label_name <- "glasser360NodeNames.txt"
  des2 <- paste0(tempdir(), "/", label_name)
  ret <- download(paste0(rpath, label_name), des2)
  
  labels <- read.table(des2, as.is=TRUE)
  cols <- t(col2rgb(rainbow(nrow(labels))))
  colnames(cols) <- c("red", "green", "blue")
  cols <- as.data.frame(cols)
  hemi <- tolower(sapply(strsplit(labels[,1], "_"), "[[", 1))
  region <- sapply(strsplit(labels[,1], "_"), "[[", 2)
  
  cids <- 1:nrow(labels)
  label_map <- as.list(cids)
  names(label_map) <- region
  
  vol <- neuroim2::ClusteredNeuroVol(as.logical(vol), clusters=vol[vol!=0], label_map=label_map)
  

  ret <- list(
    name="Glasser360",
    atlas=vol,
    cmap=cols,
    ids=1:nrow(labels),
    labels=region,
    hemi=hemi)
  
  class(ret) <- c("glasser", "atlas")
  ret
}

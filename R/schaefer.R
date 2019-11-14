
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

resample <- function(vol, outspace, smooth=FALSE) {
  assertthat::assert_that(inherits(outspace, "NeuroSpace"))
  assertthat::assert_that(length(dim(outspace)) == 3)
  cds <- index_to_coord(outspace, 1:prod(dim(outspace)))
  grid <- coord_to_grid(vol, cds) - .5

  for (i in 1:3) {
    g <- grid[,i]
    g[g < 1] = 1
    g[g > dim(vol)[i]] <- dim(vol)[i]
    grid[,i] <- g
  }
  arr2 <- vol[grid]
  vol <- NeuroVol(arr2, outspace)

  if (smooth) {
    ds <- spacing(vol)
    mask <- as.logical(vol != 0)
    sl <- neuroim2::searchlight_coords(mask, radius=min(ds)+.5, nonzero=TRUE)
    for (i in 1:length(sl)) {
      cds <- sl[[i]]
      labels <- vol[cds]
      md <- getmode(labels)
      if (md != 0) {
        vol[cds] <- md
      }
    }

    vol[mask == 0] <- 0
  }

  vol

  # arr <- vol@.Data
  # ospacing <- dim(vol) / outdim * spacing(vol)
  # im <- imager::as.cimg(arr)
  # imr <- resize(im, outdim[1], outdim[2], outdim[3], interpolation_type = 1)
  # arr2 <- drop(as.array(imr))
  # vol <- NeuroVol(arr2, NeuroSpace(outdim, ospacing))

}

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
#' Files are downloaded from the github repository: https://github.com/ThomasYeoLab/CBIG/
get_schaefer_atlas <- function(parcels=c("100","200","300","400","500","600","800","1000"),
                               networks=c("7","17"),resolution=c("1","2"), outspace=NULL, smooth=FALSE) {

  parcels <- match.arg(parcels)
  networks <- match.arg(networks)
  resolution <- match.arg(resolution)

  fname <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order_FSLMNI152_", resolution, "mm.nii.gz")

  rpath <- "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"
  path <- paste0(rpath,fname)

  des <- paste0(tempdir(), "/", fname)
  ret <- downloader::download(path, des)

  vol <- read_vol(des)

  if (!is.null(outspace)) {
    print(outspace)
    assertthat::assert_that(length(dim(outspace)) == 3)
    vol <- resample(vol, outspace, smooth)
  }

  label_name <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order.txt")
  des2 <- paste0(tempdir(), "/", label_name)
  ret <- downloader::download(paste0(rpath, label_name), des2)
  labels <- read.table(des2, as.is=TRUE)

  full_label <- labels[,2]
  labels <- labels[, 1:5]
  names(labels) <- c("ROINUM", "label", "red", "green", "blue")
  labels$label <- gsub(paste0(networks, "Networks", "_"), "", labels$label)
  hemi <- substr(labels$label, 1,2)
  labels$hemi <- hemi

  labels$hemi <- sapply(strsplit(labels$label, "_"), "[[", 1)
  labels$network <-  sapply(strsplit(labels$label, "_"), "[[", 2)
  labels$name <-   sapply(strsplit(labels$label, "_"), function(x) paste(x[(length(x)-1):length(x)], collapse="_"))

  labels$hemi[hemi == "LH"] <- "left"
  labels$hemi[hemi == "RH"] <- "right"

  ret <- list(
    name=paste0("Schaefer-", parcels, "-", networks, "networks"),
    atlas=vol,
    cmap=labels[,3:5],
    ids=1:nrow(labels),
    labels=labels$name,
    orig_labels=full_label,
    network=labels$network,
    hemi=labels$hemi)

  class(ret) <- c("schaefer", "atlas")
  ret
}



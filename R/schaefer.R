

schaefer_path <- list(
  rpath = "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"
)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

resample <- function(vol, outspace, smooth=FALSE) {
  assertthat::assert_that(inherits(outspace, "NeuroSpace"))
  assertthat::assert_that(length(dim(outspace)) == 3)
  
  vol <- neuroim2::resample(vol, outspace, interpolation=0)
  vol2 <- vol
  # cds <- index_to_coord(outspace, 1:prod(dim(outspace)))
  # grid <- coord_to_grid(vol, cds) - .5
  # 
  # for (i in 1:3) {
  #   g <- grid[,i]
  #   g[g < 1] = 1
  #   g[g > dim(vol)[i]] <- dim(vol)[i]
  #   grid[,i] <- g
  # }
  # arr2 <- vol[grid]
  # vol <- NeuroVol(arr2, outspace)
  # vol2 <- vol
  if (smooth) {
    ds <- spacing(vol)
    mask <- as.logical(vol != 0)
    sl <- neuroim2::searchlight_coords(mask, radius=max(ds)+.1, nonzero=TRUE)
    for (i in 1:length(sl)) {
      cds <- sl[[i]]

      if (nrow(cds) > 2) {
        labels <- vol[cds]
        if (all(labels[1] != labels[2:length(labels)])) {
          md <- getmode(labels)
          if (md != 0) {
            vol2[cds[1,,drop=FALSE]] <- md
          }
        }
      }
    }

    vol2[mask == 0] <- 0
    vol <- vol2
  }

  vol

  # arr <- vol@.Data
  # ospacing <- dim(vol) / outdim * spacing(vol)
  # im <- imager::as.cimg(arr)
  # imr <- resize(im, outdim[1], outdim[2], outdim[3], interpolation_type = 1)
  # arr2 <- drop(as.array(imr))
  # vol <- NeuroVol(arr2, NeuroSpace(outdim, ospacing))

}


load_schaefer_vol <- function(parcels, networks, resolution, use_cache=TRUE) {
  fname <- paste0("Schaefer2018_", parcels, "Parcels_", 
                  networks, "Networks_order_FSLMNI152_", resolution, "mm.nii.gz")
  
  vol <- if (use_cache) {
    pname <- paste0(get_cache_dir(), "/", fname)
    if (file.exists(pname)) {
      read_vol(pname)
    }
  }
  
  
  if (is.null(vol)) {
    ##fname <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order_FSLMNI152_", resolution, "mm.nii.gz")
    path <- paste0(schaefer_path$rpath,fname)
    
    des <- paste0(tempdir(), "/", fname)
    ret <- downloader::download(path, des)
    
    vol <- read_vol(des)
  
    cdir <- get_cache_dir()
    write_vol(vol, paste0(get_cache_dir(), "/", fname))
    
  }
  
  vol
  
}

load_schaefer_labels <- function(parcels, networks, use_cache=TRUE) {
  label_name <- paste0("Schaefer2018_", parcels, "Parcels_", networks, "Networks_order.txt")
  labels <- NULL
  if (use_cache) {
    if (file.exists(paste0(get_cache_dir(), "/", label_name))) {
      labels <- read.table(paste0(get_cache_dir(), "/", label_name), header=FALSE, as.is=TRUE)
    }
  }
  
  if (is.null(labels)) {
    des2 <- paste0(tempdir(), "/", label_name)
    ret <- downloader::download(paste0(schaefer_path$rpath, label_name), des2)
    labels <- read.table(des2, header=FALSE, as.is=TRUE)
    file.copy(des2, paste0(get_cache_dir(), "/", label_name), overwrite=TRUE)
  }
  
  labels
}


schaefer_metainfo <- function(parcels, networks, use_cache=TRUE) {
  #browser()
  labels = load_schaefer_labels(parcels, networks)

  #browser()
  full_label <- labels[,2]
  labels <- labels[, 1:5]
  names(labels) <- c("roinum", "label", "red", "green", "blue")
  labels$label <- gsub(paste0(networks, "Networks", "_"), "", labels$label)
  hemi <- substr(labels$label, 1,2)
  labels$hemi <- hemi

  labels$hemi <- sapply(strsplit(labels$label, "_"), "[[", 1)
  labels$network <-  sapply(strsplit(labels$label, "_"), "[[", 2)
  labels$name <-   sapply(strsplit(labels$label, "_"), function(x) paste(x[(length(x)-1):length(x)], collapse="_"))

  labels$hemi[hemi == "LH"] <- "left"
  labels$hemi[hemi == "RH"] <- "right"

  labels

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
#' @importFrom neuroim2 ClusteredNeuroVol
#' @examples
#'
#' v1 <- get_schaefer_atlas(parcels="300")
#' 
#' tr <- neuroim2::trans(v1$atlas)
#' tr[cbind(1:3, 1:3)] <- tr[cbind(1:3, 1:3)] * 2
#' v2 <- neuroim2::NeuroSpace(dim=c(91,109,91), spacing=c(2,2,2), trans=tr)
#' v3 <- get_schaefer_atlas(parcels="300", outspace=v2)
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
                               networks=c("7","17"),resolution=c("1","2"), outspace=NULL, smooth=FALSE, use_cache=TRUE) {

  parcels <- match.arg(parcels)
  networks <- match.arg(networks)
  resolution <- match.arg(resolution)

 
  vol <- load_schaefer_vol(parcels, networks, resolution, use_cache)
  
  if (!is.null(outspace)) {
    #print(outspace)
    assertthat::assert_that(length(dim(outspace)) == 3)
    vol <- resample(vol, outspace, smooth)
  }
  
  #browser()

  labels <- schaefer_metainfo(parcels, networks, use_cache)
  cids <- 1:nrow(labels)
  label_map <- as.list(cids)
  names(label_map) <- labels$name
  
  vol <- neuroim2::ClusteredNeuroVol(as.logical(vol), clusters=vol[vol!=0], label_map=label_map)

  ret <- list(
    name=paste0("Schaefer-", parcels, "-", networks, "networks"),
    atlas=vol,
    cmap=labels[,3:5],
    ids=1:nrow(labels),
    labels=labels$name,
    orig_labels=labels[,2],
    network=labels$network,
    hemi=labels$hemi)

  class(ret) <- c("schaefer", "volatlas", "atlas")
  ret
}


#' @export
get_schaefer_surfatlas <- function(parcels=c("100","200","300","400","500","600","800","1000"),
                                 networks=c("7","17"), surf=c("orig", "inflated", "white", "pial")) {


  #https://github.com/ThomasYeoLab/CBIG/blob/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage6/label/lh.Schaefer2018_1000Parcels_17Networks_order.annot

  data(fsaverage)

  get_hemi <- function(hemi) {
    fname <- paste0(hemi, ".", "Schaefer2018_", parcels, "Parcels_", networks, "Networks_order.annot")

    rpath <- "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage6/label/"
    path <- paste0(rpath,fname)

    des <- paste0(tempdir(), "/", fname)
    ret <- downloader::download(path, des)

    geom <- paste0(hemi, "_", surf)
    annot <- neurosurf::read_freesurfer_annot(des, fsaverage[[geom]])

    nrois <- as.integer(parcels)

    if (hemi == "rh") {
      annot@data <- annot@data + nrois/2
      annot@data <- annot@data - 1
      annot@data[annot@data == nrois/2] <- 0
      annot@labels <- annot@labels[-1]
    } else {
      annot@data <- annot@data - 1
      annot@labels <- annot@labels[-1]
    }

  annot

  }


  rp <-  "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"

  labels <- schaefer_metainfo(rp, parcels, networks)

  lh_surf <- get_hemi("lh")
  rh_surf <- get_hemi("rh")

  ret <- list(
    surf_type=surf,
    lh_atlas = lh_surf,
    rh_atlas = rh_surf,
    name=paste0("Schaefer-", parcels, "-", networks, "networks"),
    cmap=labels[,3:5],
    ids=1:nrow(labels),
    labels=labels$name,
    orig_labels=labels[,2],
    network=labels$network,
    hemi=labels$hemi)

  class(ret) <- c("schaefer", "surfatlas", "atlas")
  ret
}





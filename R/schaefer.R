#' Base URL for Schaefer Atlas Files
#' @keywords internal
#' @noRd
schaefer_path <- list(
  rpath = "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"
)

#' Find Mode of a Vector
#'
#' @description
#' Internal helper function to find the most frequent value in a vector.
#'
#' @param v Numeric vector
#' @return The most frequent value in the vector
#' @keywords internal
#' @noRd
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Resample Volume to New Space
#'
#' @description
#' Resamples a volume to a new space with optional smoothing of parcel boundaries.
#' This is particularly useful for atlas parcellations where maintaining discrete
#' labels is important.
#'
#' @details
#' The resampling process:
#' \itemize{
#'   \item First performs nearest-neighbor interpolation to the new space
#'   \item Optionally smooths boundaries using a local majority voting scheme
#'   \item Preserves zeros in the mask (background)
#' }
#'
#' @param vol A NeuroVol object to be resampled
#' @param outspace A NeuroSpace object specifying the target space
#' @param smooth Logical. Whether to apply boundary smoothing after resampling.
#'   Default: FALSE
#' @param interp Integer. Interpolation method (0=nearest neighbor, 1=linear).
#'   Default: 0
#' @param radius Numeric. Radius for smoothing neighborhood in voxels.
#'   If NULL, uses max(spacing)+0.5. Default: NULL
#' @param min_neighbors Integer. Minimum number of neighbors required for smoothing.
#'   Default: 3
#'
#' @return A resampled NeuroVol object in the new space
#'
#' @importFrom assertthat assert_that
#' @importFrom neuroim2 resample searchlight_coords spacing
#' @export
resample <- function(vol, outspace, smooth=FALSE, interp=0, radius=NULL,
                    min_neighbors=3) {
  # Input validation
  assertthat::assert_that(inherits(vol, "NeuroVol"),
                         msg="'vol' must be a NeuroVol object")
  assertthat::assert_that(inherits(outspace, "NeuroSpace"),
                         msg="'outspace' must be a NeuroSpace object")
  assertthat::assert_that(length(dim(outspace)) == 3,
                         msg="'outspace' must have 3 dimensions")
  assertthat::assert_that(interp %in% c(0,1),
                         msg="'interp' must be 0 (nearest) or 1 (linear)")
  if (!is.null(radius)) {
    assertthat::assert_that(radius > 0,
                           msg="'radius' must be positive")
  }
  assertthat::assert_that(min_neighbors >= 2,
                         msg="'min_neighbors' must be >= 2")

  # Store original labels for validation
  vol_data <- if (inherits(vol, "NeuroVol")) {
    vol[,,]
  } else {
    as.vector(vol)
  }
  orig_labels <- sort(unique(as.vector(vol_data[vol_data != 0])))

  # Initial resampling
  vol <- neuroim2::resample(vol, outspace, interpolation=interp)
  vol2 <- vol

  if (smooth) {
    ds <- neuroim2::spacing(vol)
    mask <- as.logical(vol != 0)

    # Set radius if not provided
    if (is.null(radius)) {
      radius <- max(ds) + 0.5
    }

    sl <- neuroim2::searchlight_coords(mask, radius=radius, nonzero=TRUE)

    for (i in 1:length(sl)) {
      cds <- sl[[i]]

      if (nrow(cds) >= min_neighbors) {
        labels <- vol[cds]
        center_label <- labels[1]
        neighbor_labels <- labels[2:length(labels)]

        # Modified smoothing logic: smooth if majority differs from center
        if (sum(neighbor_labels != center_label) > length(neighbor_labels)/2) {
          md <- getmode(labels)
          if (md != 0) {
            vol2[cds[1,,drop=FALSE]] <- md
          }
        }
      }
    }

    # Preserve mask
    vol2[mask == 0] <- 0
    vol <- vol2
  }

  # Validate output
  final_labels <- sort(unique(as.vector(vol[vol != 0])))
  if (!all(final_labels %in% orig_labels)) {
    warning("Resampling introduced new label values")
  }
  if (!all(orig_labels %in% final_labels)) {
    warning("Some original labels were lost during resampling")
  }

  vol
}

#' Load Schaefer Atlas Volume
#'
#' @description
#' Internal function to download and load Schaefer atlas volume files.
#'
#' @param parcels Number of parcels
#' @param networks Number of networks
#' @param resolution Resolution in mm
#' @param use_cache Whether to use cached files
#' @return A NeuroVol object containing the atlas
#' @keywords internal
#' @noRd
load_schaefer_vol <- function(parcels, networks, resolution, use_cache=TRUE) {
  fname <- paste0("Schaefer2018_", parcels, "Parcels_",
                 networks, "Networks_order_FSLMNI152_", resolution, "mm.nii.gz")

  vol <- if (use_cache) {
    pname <- paste0(get_cache_dir(), "/", fname)
    if (file.exists(pname)) {
      neuroim2::read_vol(pname)
    }
  }

  if (is.null(vol)) {
    path <- paste0(schaefer_path$rpath, fname)
    des <- paste0(tempdir(), "/", fname)
    ret <- downloader::download(path, des)
    vol <- neuroim2::read_vol(des)
    neuroim2::write_vol(vol, paste0(get_cache_dir(), "/", fname))
  }

  vol
}

#' @noRd
#' @keywords internal
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
    #ret <- downloader::download(paste0(schaefer_path$rpath, label_name), des2)
    message("downloading: ", paste0(schaefer_path$rpath, "/freeview_lut/", label_name))
    ret <- downloader::download(paste0(schaefer_path$rpath, "/freeview_lut/", label_name), des2)
    labels <- read.table(des2, header=FALSE, as.is=TRUE)
    file.copy(des2, paste0(get_cache_dir(), "/", label_name), overwrite=TRUE)
  }

  labels
}


#' @noRd
#' @keywords internal
schaefer_metainfo <- function(parcels, networks, use_cache=TRUE) {
  #browser()
  labels = load_schaefer_labels(parcels, networks, use_cache)

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

#' Load Schaefer Brain Parcellation Atlas
#'
#' @description
#' Retrieves and loads the Schaefer brain parcellation atlas, which provides a
#' data-driven parcellation of the cerebral cortex based on both local gradient
#' and global similarity approaches.
#'
#' @details
#' The Schaefer atlas offers multiple resolutions of cortical parcellation
#' (100-1000 parcels) and two network versions (7 or 17 networks). The atlas
#' is based on resting-state functional connectivity from 1489 subjects.
#' Features include:
#' \itemize{
#'   \item Multiple granularity levels (100-1000 parcels)
#'   \item Network assignments (7 or 17 networks)
#'   \item Bilateral parcellation
#'   \item Available in different resolutions (1mm or 2mm)
#' }
#'
#' @param parcels Character string specifying number of parcels.
#'   Options: "100", "200", "300", "400", "500", "600", "800", "1000"
#' @param networks Character string specifying network count.
#'   Options: "7", "17"
#' @param resolution Character string specifying MNI space resolution in mm.
#'   Options: "1", "2"
#' @param outspace Optional \code{NeuroSpace} object for resampling the atlas
#' @param smooth Logical. Whether to smooth parcel boundaries after resampling.
#'   Default: FALSE
#' @param use_cache Logical. Whether to cache downloaded files. Default: TRUE
#' @param ... Additional arguments (currently unused, included for consistency 
#'   with convenience functions)
#'
#' @return A list with classes c("schaefer", "volatlas", "atlas") containing:
#' \describe{
#'   \item{name}{Character string identifying atlas version}
#'   \item{atlas}{\code{ClusteredNeuroVol} object containing the parcellation}
#'   \item{cmap}{Data frame with RGB colors for visualization}
#'   \item{ids}{Integer vector of region IDs}
#'   \item{labels}{Character vector of region names}
#'   \item{orig_labels}{Original region labels from source data}
#'   \item{network}{Network assignment for each region}
#'   \item{hemi}{Hemisphere designation for each region}
#' }
#'
#' @examples
#' \dontrun{
#' # Load 300-parcel atlas with 7 networks
#' atlas <- get_schaefer_atlas(parcels = "300", networks = "7")
#'
#' # Load high-resolution version
#' atlas_hires <- get_schaefer_atlas(parcels = "400",
#'                                  networks = "17",
#'                                  resolution = "1")
#'
#' # Resample to a different space
#' new_space <- neuroim2::NeuroSpace(dim = c(91,109,91),
#'                                  spacing = c(2,2,2))
#' atlas_resampled <- get_schaefer_atlas(parcels = "300",
#'                                      outspace = new_space)
#' }
#'
#' @references
#' Schaefer, A., et al. (2018). Local-Global Parcellation of the Human Cerebral
#' Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9),
#' 3095-3114.
#'
#' @source
#' \url{https://github.com/ThomasYeoLab/CBIG/}
#'
#' @seealso
#' \code{\link{get_schaefer_surfatlas}} for surface-based version
#'
#' @section Convenience Functions:
#' Shorthand functions are provided for common Schaefer atlas configurations. These functions call \code{get_schaefer_atlas} with the \code{parcels} and \code{networks} arguments pre-set. They all accept \code{resolution} (default "2"), \code{outspace}, \code{smooth}, \code{use_cache}, and \code{...} arguments.
#' \itemize{
#'   \item \code{sy_100_7()}: 100 parcels, 7 networks.
#'   \item \code{sy_100_17()}: 100 parcels, 17 networks.
#'   \item \code{sy_200_7()}: 200 parcels, 7 networks.
#'   \item \code{sy_200_17()}: 200 parcels, 17 networks.
#'   \item \code{sy_300_7()}: 300 parcels, 7 networks.
#'   \item \code{sy_300_17()}: 300 parcels, 17 networks.
#'   \item \code{sy_400_7()}: 400 parcels, 7 networks.
#'   \item \code{sy_400_17()}: 400 parcels, 17 networks.
#'   \item \code{sy_500_7()}: 500 parcels, 7 networks.
#'   \item \code{sy_500_17()}: 500 parcels, 17 networks.
#'   \item \code{sy_600_7()}: 600 parcels, 7 networks.
#'   \item \code{sy_600_17()}: 600 parcels, 17 networks.
#'   \item \code{sy_800_7()}: 800 parcels, 7 networks.
#'   \item \code{sy_800_17()}: 800 parcels, 17 networks.
#'   \item \code{sy_1000_7()}: 1000 parcels, 7 networks.
#'   \item \code{sy_1000_17()}: 1000 parcels, 17 networks.
#' }
#'
#' @importFrom neuroim2 read_vol ClusteredNeuroVol write_vol
#' @importFrom downloader download
#' @importFrom assertthat assert_that
#' @importFrom utils read.table
#'
#' @export
get_schaefer_atlas <- function(parcels=c("100","200","300","400","500","600","700","800","900","1000"),
                              networks=c("7","17"), resolution=c("1","2"),
                              outspace=NULL, smooth=FALSE, use_cache=TRUE) {

  parcels <- match.arg(as.character(parcels), 
                      choices = c("100","200","300","400","500","600","700","800","900","1000"))
  networks <- match.arg(as.character(networks), 
                       choices = c("7","17"))
  resolution <- match.arg(as.character(resolution), 
                         choices = c("1","2"))

  # Resolve outspace if it's not NULL and not already a NeuroSpace (T6.1.4)
  if (!is.null(outspace) && !methods::is(outspace, "NeuroSpace")) {
    message("Attempting to resolve 'outspace' argument via TemplateFlow...")
    # We need .resolve_template_input to be available. 
    # Assuming it's exported from neuroatlas or accessible.
    # If it's internal, this call would need neuroatlas:::.resolve_template_input
    # For now, assuming it becomes an exported utility or is otherwise accessible.
    # If this file is part of the same package, direct call might work if NAMESPACE handles it.
    resolved_outspace <- tryCatch({
      .resolve_template_input(outspace, target_type = "NeuroSpace")
    }, error = function(e) {
      stop("Failed to resolve 'outspace' via TemplateFlow: ", conditionMessage(e),
           "\n'outspace' must be a NeuroSpace object, a TemplateFlow space ID string, or a list of get_template() arguments.")
      return(NULL) # Should be caught by stop
    })
    
    if (is.null(resolved_outspace) || !methods::is(resolved_outspace, "NeuroSpace")) {
        stop("Resolution of 'outspace' did not result in a valid NeuroSpace object.")
    }
    outspace <- resolved_outspace # Replace original outspace with the resolved NeuroSpace
  }

  vol <- load_schaefer_vol(parcels, networks, resolution, use_cache)

  if (!is.null(outspace)) {
    #print(outspace)
    assertthat::assert_that(length(dim(outspace)) == 3)
    vol <- resample(vol, outspace, smooth)
  }



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


#' Load Surface-Based Schaefer Atlas
#'
#' @description
#' Loads the surface-based version of the Schaefer parcellation atlas, compatible
#' with FreeSurfer surface representations.
#'
#' @details
#' Provides the Schaefer parcellation mapped to FreeSurfer surface meshes. The
#' atlas can be loaded onto different surface representations (inflated, white,
#' or pial) and maintains the same parcellation scheme as the volumetric version.
#'
#' @param parcels Character string specifying number of parcels.
#'   Options: "100", "200", "300", "400", "500", "600", "800", "1000"
#' @param networks Character string specifying network count.
#'   Options: "7", "17"
#' @param surf Character string specifying surface type.
#'   Options: "inflated", "white", "pial"
#' @param use_cache Logical. Whether to cache downloaded files. Default: TRUE
#'
#' @return A list with classes c("schaefer", "surfatlas", "atlas") containing:
#' \describe{
#'   \item{surf_type}{Surface type used}
#'   \item{lh_atlas}{Left hemisphere surface atlas}
#'   \item{rh_atlas}{Right hemisphere surface atlas}
#'   \item{name}{Atlas identifier}
#'   \item{cmap}{RGB color specifications}
#'   \item{ids}{Region IDs}
#'   \item{labels}{Region names}
#'   \item{orig_labels}{Original region labels}
#'   \item{network}{Network assignments}
#'   \item{hemi}{Hemisphere designations}
#' }
#'
#' @examples
#' \dontrun{
#' # Load inflated surface atlas
#' surf_atlas <- get_schaefer_surfatlas(parcels = "300",
#'                                     networks = "7",
#'                                     surf = "inflated")
#'
#' # Load pial surface version
#' pial_atlas <- get_schaefer_surfatlas(parcels = "400",
#'                                     networks = "17",
#'                                     surf = "pial")
#' }
#'
#' @seealso
#' \code{\link{get_schaefer_atlas}} for volumetric version
#'
#' @importFrom neurosurf read_freesurfer_annot
#' @importFrom downloader download
#' @importFrom utils data
#' @export
get_schaefer_surfatlas <- function(parcels=c("100","200","300","400","500","600","800","1000"),
                                  networks=c("7","17"), surf=c("inflated", "white", "pial"),
                                  use_cache=TRUE) {


  #https://github.com/ThomasYeoLab/CBIG/blob/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage6/label/lh.Schaefer2018_1000Parcels_17Networks_order.annot

  parcels <- match.arg(parcels)
  networks <- match.arg(networks)
  surf <- match.arg(surf)
  #resolution <- match.arg(resolution)

  fsaverage <- NULL  # To avoid R CMD check NOTE
  utils::data("fsaverage", envir = environment())

  get_hemi <- function(hemi) {

    fname <- paste0(hemi, ".", "Schaefer2018_", parcels, "Parcels_", networks, "Networks_order.annot")

    rpath <- "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage6/label/"
    path <- paste0(rpath,fname)

    des <- paste0(tempdir(), "/", fname)
    ret <- downloader::download(path, des)

    geom <- paste0(hemi, "_", surf)
    annot <- suppressWarnings(neurosurf::read_freesurfer_annot(des, fsaverage[[geom]]))

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

    class(annot) <- c(class(annot), "surf_atlas")
    annot

  }


  ##rp <-  "https://raw.githubusercontent.com/ThomasYeoLab/CBIG/master/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/MNI/"

  labels <- schaefer_metainfo(parcels, networks, use_cache=use_cache)
  #browser()
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



#' @rdname get_schaefer_atlas
#' @export
sy_100_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "100", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_100_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "100", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_200_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "200", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_200_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "200", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_300_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "300", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_300_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "300", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_400_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "400", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_400_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "400", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_500_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "500", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_500_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "500", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_600_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "600", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_600_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "600", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_700_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "700", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_700_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "700", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_800_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "800", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_800_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "800", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_900_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "900", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_900_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "900", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_1000_7 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "1000", networks = "7", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}

#' @rdname get_schaefer_atlas
#' @export
sy_1000_17 <- function(resolution = "2", outspace = NULL, smooth = FALSE, use_cache = TRUE, ...) {
  get_schaefer_atlas(parcels = "1000", networks = "17", resolution = resolution,
                     outspace = outspace, smooth = smooth, use_cache = use_cache, ...)
}





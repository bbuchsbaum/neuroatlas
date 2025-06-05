#' @rdname print-methods
#' @param x An object of class 'atlas'
#' @param ... Additional arguments passed to print methods
#' @describeIn print Print method for atlas objects.
#' @importFrom crayon bold green blue red white
#' @importFrom cli rule symbol
#' @export
print.atlas <- function(x, ...) {
  # Header
  cat(cli::rule(left = crayon::bold("Atlas Summary"), col = "cyan", width = 60), "\n\n")
  
  # Basic info
  cat(crayon::blue(cli::symbol$pointer), " ", 
      crayon::bold("Name:   "), crayon::white(x$name), "\n", sep="")
  
  # Volume info
  dims <- dim(x$atlas)
  cat(crayon::blue(cli::symbol$pointer), " ",
      crayon::bold("Dimensions: "), 
      crayon::white(paste0(dims[1], " × ", dims[2], " × ", dims[3])), "\n", sep="")
  
  # Region counts
  cat(crayon::blue(cli::symbol$pointer), " ",
      crayon::bold("Regions: "), 
      crayon::green(length(x$ids)), "\n", sep="")
  
  # Hemisphere breakdown
  left_count <- sum(x$hemi == "left", na.rm=TRUE)
  right_count <- sum(x$hemi == "right", na.rm=TRUE)
  bilateral_count <- sum(is.na(x$hemi))
  
  cat("\n", crayon::bold("Structure Distribution:"), "\n", sep="")
  cat(crayon::red("├─"), " Left hemisphere:     ", 
      crayon::white(left_count), "\n", sep="")
  cat(crayon::red("├─"), " Right hemisphere:    ", 
      crayon::white(right_count), "\n", sep="")
  cat(crayon::red("└─"), " Bilateral/Midline:   ", 
      crayon::white(bilateral_count), "\n", sep="")
  
  # Footer
  cat("\n", cli::rule(col = "cyan", width = 60), "\n", sep="")
}

#' Create Cache Directory for Atlas Data
#'
#' @description
#' Creates a hidden directory in the user's home folder for caching atlas data.
#'
#' @return Character string containing the path to the cache directory
#' @keywords internal
#' @noRd
create_cache_dir <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  if (!dir.exists(dname)) {
    dir.create(dname)
  }
  dname
}

#' Get Cache Directory Path
#'
#' @description
#' Returns the path to the atlas cache directory, creating it if necessary.
#'
#' @return Character string containing the path to the cache directory
#' @keywords internal
#' @noRd
get_cache_dir <- function() {
  create_cache_dir()
}

#' Clear Atlas Cache
#'
#' @description
#' Removes all cached atlas files from the cache directory.
#'
#' @return None
#' @keywords internal
#' @noRd
clear_cache <- function() {
  dname <- paste0(Sys.getenv("HOME"), "/.neuroatlas_cache")
  fnames <- list.files(dname, full.names=TRUE)
  sapply(fnames, unlink)
}

#' Merge Two Brain Atlases
#'
#' @description
#' Combines two brain atlases into a single unified atlas object, preserving all
#' region information and adjusting region IDs to prevent conflicts. This is useful
#' for creating composite atlases that combine different parcellation schemes.
#'
#' @details
#' The merging process:
#' \itemize{
#'   \item Verifies that both atlases have the same dimensions
#'   \item Adjusts region IDs in the second atlas to avoid overlap
#'   \item Combines color maps, labels, and hemisphere information
#'   \item Creates a new ClusteredNeuroVol object for the merged atlas
#' }
#'
#' @param atlas1 The first atlas object to merge
#' @param atlas2 The second atlas object to merge
#'
#' @return A new atlas object containing:
#' \describe{
#'   \item{name}{Combined names of input atlases (atlas1::atlas2)}
#'   \item{atlas}{Combined \code{ClusteredNeuroVol} object}
#'   \item{cmap}{Combined colormap for all regions}
#'   \item{ids}{Adjusted vector of all region IDs}
#'   \item{labels}{Combined vector of region labels}
#'   \item{orig_labels}{Original labels from both atlases}
#'   \item{hemi}{Combined hemisphere designations}
#' }
#'
#' @examples
#' \dontrun{
#' # Load two atlases
#' atlas1 <- get_aseg_atlas()
#' atlas2 <- get_aseg_atlas()
#'
#' # Merge the atlases
#' merged <- merge_atlases(atlas1, atlas2)
#'
#' # Check the combined regions
#' print(merged)
#' }
#'
#' @seealso
#' \code{\link{get_aseg_atlas}}, \code{\link{get_roi}}
#'
#' @importFrom assertthat assert_that
#' @importFrom neuroim2 NeuroVol ClusteredNeuroVol space
#' @export
merge_atlases <- function(atlas1, atlas2) {
  assertthat::assert_that(all(dim(atlas1$atlas) == dim(atlas2$atlas)))

  atl2 <- atlas2$atlas
  atl2[atl2 != 0] <- atl2[atl2 != 0] + max(atlas1$ids) 
  atlmerged <- neuroim2::NeuroVol(as.numeric(atlas1$atlas@data), space=space(atlas1$atlas))
  
  
  atlmerged[atl2 != 0] <- atl2[atl2 != 0]
  
  #vol <- neuroim2::ClusteredNeuroVol(as.logical(vol), clusters=vol[vol!=0], 
  #                                   label_map=label_map)
  
  ids <- atlmerged[atlmerged != 0]
  cids <- 1:length(unique(ids))
  label_map <- as.list(cids)
  names(label_map) <- c(atlas1$orig_labels, atlas2$orig_labels)
  
  atlmerged <- neuroim2::ClusteredNeuroVol(as.logical(atlmerged),
                                           clusters=atlmerged[atlmerged != 0],
                                           label_map=label_map)
                                           
  

  ret <- list(
    name=paste0(atlas1$name,"::", atlas2$name),
    atlas=atlmerged,
    cmap=rbind(atlas1$cmap, atlas2$cmap),
    ids=c(atlas1$ids, atlas2$ids + max(atlas1$ids) + 1),
    labels=c(atlas1$labels, atlas2$labels),
    orig_labels=c(atlas1$orig_labels, atlas2$orig_labels),
    hemi=c(atlas1$hemi, atlas2$hemi)
  )

  class(ret) <- c(paste0(atlas1$name,"::", atlas2$name), "atlas")
  ret
}


#' @rdname get_roi-methods
#' @inheritParams get_roi
#' @importFrom neuroim2 space ROIVol index_to_grid
#' @describeIn get_roi Extract ROI from a generic atlas object.
#' @export
get_roi.atlas <- function(x, label, id=NULL, hemi=NULL) {
  if (!is.null(label) && !is.null(id)) {
    stop("must supply one of 'id' or 'label' but not both")
  }

  if (!is.null(label)) {
    ret <- lapply(label, function(l) {
      id <- x$ids[which(x$labels == l)]
      if (length(id) == 0) {
        stop(paste0("label '", l, "' not found in atlas"))
      }
      rind <- which(x$atlas %in% id)
      neuroim2::ROIVol(neuroim2::space(x$atlas),
                       coords = neuroim2::index_to_grid(x$atlas, rind),
                       data=x$atlas[rind])
    })

    names(ret) <- label
    ret
  } else {
    ret <- lapply(id, function(i) {
      rind <- which(x$atlas %in% i)
      neuroim2::ROIVol(neuroim2::space(x$atlas),coords = neuroim2::index_to_grid(x$atlas, rind),data=i)
    })
    names(ret) <- id
    ret
  }
}

#' @rdname reduce_atlas
#' @inheritParams reduce_atlas
#'
#' When \code{data_vol} is a 3D \code{NeuroVol}, the returned tibble contains a
#' single row with one column per ROI. If a 4D \code{NeuroVec} is supplied, each
#' time point is summarised separately and a \code{time} column is added to the
#' tibble.
#'
#' @export
#' @method reduce_atlas atlas
reduce_atlas.atlas <- function(atlas, data_vol, stat_func, ...) {

  # --- Input Validation ---
  if (!methods::is(data_vol, "NeuroVol") && !methods::is(data_vol, "NeuroVec")) {
    stop("'data_vol' must be a NeuroVol or NeuroVec object.")
  }
  if (!is.function(stat_func)) {
    stop("'stat_func' must be a function.")
  }

  # --- Determine ROI definition volume from 'atlas' (which is an 'atlas' object) ---
  # For an 'atlas' class object, the ROI definition is expected in atlas$atlas
  roi_definition_vol <- NULL
  if (!is.null(atlas$atlas) && methods::is(atlas$atlas, "NeuroVol")) {
    roi_definition_vol <- atlas$atlas
  } else if (!is.null(atlas$data) && methods::is(atlas$data, "NeuroVol")) { 
    # This handles Glasser-like structures if they are passed directly and are also of class 'atlas'
    # However, typically for a well-defined 'atlas' object, atlas$atlas is primary.
    # Consider if Glasser objects should have their own S3 method reduce_atlas.glasser_atlas if structure differs significantly
    # For now, this provides a fallback if atlas$atlas is not present but atlas$data is.
    roi_definition_vol <- atlas$data
  }

  if (is.null(roi_definition_vol)) {
    stop("Could not determine the ROI definition volume from the input 'atlas' object. Expected a NeuroVol in element 'atlas$atlas' or 'atlas$data'.")
  }

  # --- Extract data using neuroim2::extract_roi_data ---
  extracted_values <- neuroim2::extract_roi_data(data_vol, roi_definition_vol, fun = stat_func, ...)

  # --- Convert to tibble ---
  result_tibble <- NULL
  if (is.vector(extracted_values)) {
    result_tibble <- tibble::as_tibble(as.list(extracted_values))
  } else if (is.matrix(extracted_values)) {
    transposed_matrix <- t(extracted_values)
    result_tibble <- tibble::as_tibble(transposed_matrix)
    result_tibble <- tibble::add_column(result_tibble, time = 1:nrow(result_tibble), .before = TRUE)
  } else {
    stop("Unexpected output format from neuroim2::extract_roi_data. Expected vector or matrix.")
  }

  return(result_tibble)
}



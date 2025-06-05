#' @rdname print-methods
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
      crayon::white(paste0(dims[1], " x ", dims[2], " x ", dims[3])), "\n", sep="")
  
  # Region counts
  cat(crayon::blue(cli::symbol$pointer), " ",
      crayon::bold("Regions: "), 
      crayon::green(length(x$ids)), "\n", sep="")
  
  # Hemisphere breakdown
  left_count <- sum(x$hemi == "left", na.rm=TRUE)
  right_count <- sum(x$hemi == "right", na.rm=TRUE)
  bilateral_count <- sum(is.na(x$hemi))
  
  cat("\n", crayon::bold("Structure Distribution:"), "\n", sep="")
  cat(crayon::red("|-"), " Left hemisphere:     ", 
      crayon::white(left_count), "\n", sep="")
  cat(crayon::red("|-"), " Right hemisphere:    ", 
      crayon::white(right_count), "\n", sep="")
  cat(crayon::red("\\-"), " Bilateral/Midline:   ", 
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

  # Start with atlas1 as base
  # Handle different atlas types
  if (inherits(atlas1$atlas, "ClusteredNeuroVol")) {
    # For ClusteredNeuroVol, we need to reconstruct the full volume
    atlmerged <- neuroim2::NeuroVol(array(0, dim=dim(atlas1$atlas)), 
                                    space=neuroim2::space(atlas1$atlas))
    # Fill in the cluster values
    atlmerged[atlas1$atlas != 0] <- atlas1$atlas[atlas1$atlas != 0]
  } else {
    atlmerged <- neuroim2::NeuroVol(as.numeric(as.vector(atlas1$atlas)), 
                                    space=neuroim2::space(atlas1$atlas))
  }
  
  # Create mapping for atlas2 values
  # We need to map each unique value in atlas2 to a new value that doesn't conflict with atlas1
  atl2 <- atlas2$atlas
  atl2_data <- if (inherits(atl2, "NeuroVol")) {
    extracted <- atl2[,,]
    # Convert sparse vectors to regular numeric
    if (inherits(extracted, "sparseVector")) {
      as.numeric(extracted)
    } else {
      extracted
    }
  } else {
    as.vector(atl2)
  }
  atl2_vals <- sort(unique(as.vector(atl2_data[atl2_data != 0])))
  
  # Create a remapping - shift all atlas2 values to come after atlas1's max ID
  max_atlas1_id <- max(atlas1$ids)
  remap <- list()
  shifted_ids <- atlas2$ids  # Start with original IDs
  
  # Create mapping for each unique value in atlas2
  for (i in seq_along(atl2_vals)) {
    old_val <- atl2_vals[i]
    new_val <- max_atlas1_id + i
    remap[[as.character(old_val)]] <- new_val
  }
  
  # Update shifted_ids based on the remapping
  for (i in seq_along(atlas2$ids)) {
    old_id <- atlas2$ids[i]
    if (as.character(old_id) %in% names(remap)) {
      shifted_ids[i] <- remap[[as.character(old_id)]]
    }
  }
  
  # Apply the remapping to atlas2
  atl2_remapped <- atl2
  if (inherits(atl2_remapped, "NeuroVol")) {
    # For NeuroVol objects, we need to extract, modify, and recreate
    atl2_data <- atl2_remapped[,,]
    for (old_val in names(remap)) {
      atl2_data[atl2_data == as.numeric(old_val)] <- remap[[old_val]]
    }
    # Convert to regular numeric vector if it's sparse
    if (inherits(atl2_data, "sparseVector")) {
      atl2_data <- as.numeric(atl2_data)
    }
    atl2_remapped <- neuroim2::NeuroVol(atl2_data, space = neuroim2::space(atl2))
  } else {
    # For regular arrays/vectors
    for (old_val in names(remap)) {
      atl2_remapped[atl2_remapped == as.numeric(old_val)] <- remap[[old_val]]
    }
  }
  
  # Merge the remapped atlas2 into atlmerged
  if (inherits(atlmerged, "NeuroVol") && inherits(atl2_remapped, "NeuroVol")) {
    # Extract data, modify, and recreate
    atlmerged_data <- atlmerged[,,]
    atl2_remapped_data <- atl2_remapped[,,]
    mask <- atl2_remapped_data != 0
    atlmerged_data[mask] <- atl2_remapped_data[mask]
    atlmerged <- neuroim2::NeuroVol(atlmerged_data, space = neuroim2::space(atlmerged))
  } else {
    # For regular arrays
    atlmerged[atl2_remapped != 0] <- atl2_remapped[atl2_remapped != 0]
  }
  
  # Get unique cluster values from the merged atlas
  if (inherits(atlmerged, "NeuroVol")) {
    atlmerged_data <- atlmerged[,,]
    unique_clusters <- sort(unique(as.vector(atlmerged_data[atlmerged_data != 0])))
  } else {
    unique_clusters <- sort(unique(atlmerged[atlmerged != 0]))
  }
  
  # Create a LogicalNeuroVol mask from the merged atlas
  mask <- neuroim2::LogicalNeuroVol(atlmerged != 0, space=neuroim2::space(atlmerged))
  
  # Get unique cluster values from the merged atlas (excluding 0)
  cluster_values <- atlmerged[atlmerged != 0]
  unique_clusters <- sort(unique(as.vector(cluster_values)))
  
  # Skip label_map since it causes issues with duplicate labels
  # The atlas object already maintains the mapping between IDs and labels
  atlmerged <- neuroim2::ClusteredNeuroVol(mask=mask,
                                           clusters=cluster_values)
                                           
  

  ret <- list(
    name=paste0(atlas1$name,"::", atlas2$name),
    atlas=atlmerged,
    cmap=rbind(atlas1$cmap, atlas2$cmap),
    ids=c(atlas1$ids, shifted_ids),
    labels=c(atlas1$labels, atlas2$labels),
    orig_labels=c(atlas1$orig_labels, atlas2$orig_labels),
    hemi=c(atlas1$hemi, atlas2$hemi)
  )

  class(ret) <- c(paste0(atlas1$name,"::", atlas2$name), "atlas")
  ret
}


#' @rdname get_roi
#' @importFrom neuroim2 space ROIVol index_to_grid
#' @export
get_roi.atlas <- function(x, label=NULL, id=NULL, hemi=NULL) {
  if (!is.null(label) && !is.null(id)) {
    stop("must supply one of 'id' or 'label' but not both")
  }
  
  if (is.null(label) && is.null(id)) {
    stop("must supply either 'id' or 'label'")
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
      neuroim2::ROIVol(neuroim2::space(x$atlas),
                       coords = neuroim2::index_to_grid(x$atlas, rind),
                       data = x$atlas[rind])
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

  # --- Determine ROI definition volume from 'atlas' ---
  roi_definition_vol <- .get_atlas_volume(atlas)

  # --- Ensure data_vol and ROI definition share spatial dimensions ---
  atlas_dims <- dim(roi_definition_vol)[1:3]
  data_dims <- dim(data_vol)[1:3]
  if (!all(atlas_dims == data_dims)) {
    stop("Dimensions of atlas (", paste(atlas_dims, collapse="x"),
         ") do not match dimensions of data volume (", paste(data_dims, collapse="x"), ")")
  }

  # --- Extract data using ROI matching ---
  # Get unique ROI labels (excluding 0/background)
  roi_vol_data <- if (inherits(roi_definition_vol, "NeuroVol")) {
    roi_definition_vol[,,]
  } else {
    as.vector(roi_definition_vol)
  }
  roi_labels <- sort(unique(as.vector(roi_vol_data)))
  roi_labels <- roi_labels[roi_labels != 0]
  
  # Extract values for each ROI
  if (inherits(data_vol, "NeuroVol")) {
    # 3D data
    data_vol_data <- data_vol[,,]
    extracted_values <- sapply(roi_labels, function(label) {
      mask <- as.vector(roi_vol_data == label)
      roi_data <- as.vector(data_vol_data)[mask]
      if (length(roi_data) > 0) {
        stat_func(roi_data, ...)
      } else {
        NA
      }
    })
    names(extracted_values) <- roi_labels
  } else if (inherits(data_vol, "NeuroVec")) {
    # 4D data - extract time series for each ROI
    nvol <- dim(data_vol)[4]  # number of time points
    extracted_values <- matrix(NA, nrow = nvol, ncol = length(roi_labels))
    
    for (i in seq_along(roi_labels)) {
      label <- roi_labels[i]
      mask <- as.logical(roi_vol_data == label)
      
      # Extract time series for this ROI
      roi_ts <- numeric(nvol)
      for (t in 1:nvol) {
        vol_t <- data_vol[,,,t]
        # Convert mask to array indices for subsetting
        roi_data <- vol_t[which(mask)]
        if (length(roi_data) > 0) {
          roi_ts[t] <- stat_func(roi_data, ...)
        } else {
          roi_ts[t] <- NA
        }
      }
      extracted_values[, i] <- roi_ts
    }
    colnames(extracted_values) <- as.character(roi_labels)
  } else {
    stop("data_vol must be a NeuroVol or NeuroVec object")
  }

  id_to_label <- NULL
  if (!is.null(atlas$orig_labels)) {
    id_to_label <- setNames(as.character(atlas$orig_labels), atlas$ids)
  } else if (!is.null(atlas$labels)) {
    id_to_label <- setNames(as.character(atlas$labels), atlas$ids)
  }

  region_labels <- NULL
  if (!is.null(id_to_label)) {
    region_labels <- id_to_label[as.character(roi_labels)]
  }

  # --- Convert to tibble ---
  if (is.vector(extracted_values)) {
    # For 3D data, return a tibble with region id/label and value columns
    result_tibble <- tibble::tibble(
      region_id = roi_labels,
      value = as.numeric(extracted_values)
    )
    if (!is.null(region_labels)) {
      result_tibble <- tibble::add_column(result_tibble, label = region_labels, .after = "region_id")
    }
  } else if (is.matrix(extracted_values)) {
    # For 4D data, return a tibble with time as rows and regions as columns
    if (!is.null(region_labels)) {
      colnames(extracted_values) <- region_labels
    } else {
      colnames(extracted_values) <- as.character(roi_labels)
    }
    result_tibble <- tibble::as_tibble(extracted_values)
    result_tibble <- tibble::add_column(result_tibble, time = 1:nrow(result_tibble), .before = TRUE)
  } else {
    stop("Unexpected output format from stat_func. Expected vector or matrix.")
  }

  return(result_tibble)
}



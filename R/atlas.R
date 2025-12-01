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

  vol_to_array <- function(x) {
    if (methods::is(x, "NeuroVol") || methods::is(x, "ClusteredNeuroVol")) {
      methods::as(x, "array")
    } else if (is.array(x)) {
      x
    } else {
      stop("Unsupported atlas volume type: ", class(x))
    }
  }

  a1 <- vol_to_array(atlas1$atlas)
  a2 <- vol_to_array(atlas2$atlas)

  max_atlas1_id <- if (length(atlas1$ids)) max(atlas1$ids) else 0L

  a2_vals <- sort(unique(as.integer(a2[a2 != 0])))
  remap <- if (length(a2_vals)) {
    stats::setNames(max_atlas1_id + seq_along(a2_vals), as.character(a2_vals))
  } else {
    integer(0)
  }

  a2_remapped <- a2
  if (length(remap)) {
    for (old_val in names(remap)) {
      a2_remapped[a2_remapped == as.integer(old_val)] <- remap[[old_val]]
    }
  }

  merged_array <- a1
  mask <- a2_remapped != 0
  merged_array[mask] <- a2_remapped[mask]

  merged_mask <- merged_array != 0
  mask_vol <- neuroim2::LogicalNeuroVol(merged_mask, neuroim2::space(atlas1$atlas))
  cluster_values <- as.integer(merged_array[merged_mask])
  atlmerged <- neuroim2::ClusteredNeuroVol(mask = mask_vol, clusters = cluster_values)

  shifted_ids <- integer(length(atlas2$ids))
  if (length(shifted_ids)) {
    mapped <- remap[as.character(atlas2$ids)]
    shifted_ids <- as.integer(ifelse(is.na(mapped),
                                     atlas2$ids + max_atlas1_id,
                                     mapped))
  }



  cmap <- rbind(atlas1$cmap, atlas2$cmap)
  if (nrow(cmap) == length(c(atlas1$ids, shifted_ids))) {
    rownames(cmap) <- c(atlas1$ids, shifted_ids)
  }

  ret <- list(
    name=paste0(atlas1$name,"::", atlas2$name),
    atlas=atlmerged,
    cmap=cmap,
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
#'
#' @details
#' When \code{data_vol} is a 3D \code{NeuroVol}, the returned tibble contains a
#' single row with one column per ROI. If a 4D \code{NeuroVec} is supplied, each
#' time point is summarised separately and a \code{time} column is added to the
#' tibble.
#'
#' @importFrom stats setNames
#'
#' @export
#' @method reduce_atlas atlas
reduce_atlas.atlas <- function(atlas, data_vol, stat_func, ..., format = NULL) {

  # --- Input Validation ---
  if (!methods::is(data_vol, "NeuroVol") && !methods::is(data_vol, "NeuroVec")) {
    stop("'data_vol' must be a NeuroVol or NeuroVec object.")
  }
  if (!is.function(stat_func)) {
    stop("'stat_func' must be a function.")
  }

  # --- Determine ROI definition volume from 'atlas' ---
  roi_definition_vol <- .get_atlas_volume(atlas)

  # --- Ensure data_vol and ROI definition share spatial dimensions (ignore time) ---
  atlas_dims <- dim(roi_definition_vol)[1:3]
  data_dims <- dim(data_vol)[1:3]
  if (!all(atlas_dims == data_dims)) {
    stop("Dimensions of atlas (", paste(atlas_dims, collapse="x"),
         ") do not match dimensions of data volume (", paste(data_dims, collapse="x"), ")")
  }

  # --- Extract data using ROI matching ---
  # Get unique ROI labels (excluding 0/background)
  if (inherits(roi_definition_vol, "ClusteredNeuroVol")) {
    # For ClusteredNeuroVol, get labels from clusters
    roi_labels <- sort(unique(roi_definition_vol@clusters))
    roi_labels <- roi_labels[roi_labels != 0]
    # Create a full volume for masking
    roi_vol_data <- array(0, dim = dim(roi_definition_vol))
    roi_vol_data[which(roi_definition_vol@mask)] <- roi_definition_vol@clusters
  } else {
    # For regular NeuroVol
    roi_vol_data <- roi_definition_vol[,,]
    roi_labels <- sort(unique(as.vector(roi_vol_data)))
    roi_labels <- roi_labels[roi_labels != 0]
  }

  dots <- list(...)
  apply_stat <- function(x) {
    na_rm <- isTRUE(dots$na.rm)
    if (na_rm) {
      x <- x[!is.na(x)]
    }
    if (length(x) == 0) {
      return(NA_real_)
    }
    do.call(stat_func, c(list(x), dots))
  }

  # Extract values for each ROI
  if (inherits(data_vol, "NeuroVol")) {
    # 3D data -> single row with one column per ROI
    data_vol_data <- data_vol[,,]
    extracted_values <- sapply(roi_labels, function(label) {
      mask <- roi_vol_data == label
      roi_data <- data_vol_data[mask]
      apply_stat(roi_data)
    })
    extracted_values <- matrix(extracted_values, nrow = 1)
    colnames(extracted_values) <- as.character(roi_labels)
  } else if (inherits(data_vol, "NeuroVec")) {
    # 4D data -> each row is a time point
    nvol <- dim(data_vol)[4]
    extracted_values <- matrix(NA, nrow = nvol, ncol = length(roi_labels))

    for (i in seq_along(roi_labels)) {
      label <- roi_labels[i]
      mask <- as.logical(roi_vol_data == label)

      # Extract time series for this ROI
      for (t in 1:nvol) {
        vol_t <- data_vol[,,,t]
        # Convert mask to array indices for subsetting
        roi_data <- vol_t[which(mask)]
        extracted_values[t, i] <- apply_stat(roi_data)
      }
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

  if (!is.null(id_to_label)) {
    region_labels <- id_to_label[as.character(colnames(extracted_values))]
    # Only update non-NA labels
    valid_labels <- !is.na(region_labels)
    if (any(valid_labels)) {
      colnames(extracted_values)[valid_labels] <- region_labels[valid_labels]
    }
  }

  # --- Determine output format ---
  if (is.null(format)) {
    # Default: long for NeuroVol, wide for NeuroVec
    format <- if (inherits(data_vol, "NeuroVol")) "long" else "wide"
  }
  
  format <- match.arg(format, c("wide", "long"))
  
  # --- Convert to tibble ---
  if (format == "wide") {
    # Wide format (current behavior)
    if (nrow(extracted_values) == 1) {
      result_tibble <- tibble::as_tibble(extracted_values, .name_repair = "minimal")
    } else {
      result_tibble <- tibble::as_tibble(extracted_values, .name_repair = "minimal")
      result_tibble <- tibble::add_column(result_tibble,
                                          time = seq_len(nrow(result_tibble)),
                                          .before = TRUE,
                                          .name_repair = "minimal")
    }
  } else {
    # Long format
    if (nrow(extracted_values) == 1) {
      # NeuroVol: region, value
      result_tibble <- tibble::tibble(
        region = colnames(extracted_values),
        value = as.numeric(extracted_values[1, ])
      )
    } else {
      # NeuroVec: time, region, value
      long_data <- expand.grid(
        time = seq_len(nrow(extracted_values)),
        region = colnames(extracted_values),
        stringsAsFactors = FALSE
      )
      long_data$value <- as.numeric(t(extracted_values))
      result_tibble <- tibble::as_tibble(long_data)
    }
  }

  return(result_tibble)
}

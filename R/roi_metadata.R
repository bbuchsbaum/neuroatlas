#' @title ROI Metadata Functions
#' @description Functions for accessing and filtering ROI (Region of Interest)
#'   metadata in atlas objects. These functions provide a unified, discoverable
#'   interface for working with multiple ROI attributes across different atlas types.
#' @name roi_metadata
NULL

#' Get ROI Metadata for an Atlas
#'
#' @description
#' Returns a tibble containing metadata for all regions of interest (ROIs) in an atlas.
#' This provides a unified, tidy interface for accessing ROI attributes across
#' different atlas types.
#'
#' @param x An atlas object
#' @param ... Additional arguments passed to methods
#'
#' @return A tibble with one row per ROI and columns for each attribute.
#'   Standard columns include:
#'   \describe{
#'     \item{id}{Numeric ROI identifier}
#'     \item{label}{Simplified region name}
#'     \item{label_full}{Original/full region label}
#'     \item{hemi}{Hemisphere ("left", "right", or NA for bilateral/midline)}
#'     \item{color_r, color_g, color_b}{RGB color values (0-255)}
#'   }
#'   Additional atlas-specific columns may be present (e.g., \code{network} for Schaefer atlases).
#'
#' @examples
#' \dontrun{
#' # Get metadata for Schaefer atlas
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' meta <- roi_metadata(atlas)
#'
#' # Filter using dplyr
#' library(dplyr)
#' left_visual <- meta %>% filter(hemi == "left", network == "Vis")
#' }
#'
#' @seealso \code{\link{roi_attributes}} for listing available attributes,
#'   \code{\link{filter_atlas}} for filtering atlas objects by attributes
#'
#' @export
roi_metadata <- function(x, ...) {
  UseMethod("roi_metadata")
}

#' @rdname roi_metadata
#' @export
roi_metadata.atlas <- function(x, ...) {
  # If roi_metadata already exists as a field, return it

if (!is.null(x$roi_metadata)) {
    return(x$roi_metadata)
  }

  # Otherwise, construct from legacy fields for backwards compatibility
  n <- length(x$ids)

  # Handle color map - may be matrix, data.frame, or have different column names
  color_r <- color_g <- color_b <- rep(NA_integer_, n)
  if (!is.null(x$cmap) && nrow(x$cmap) == n) {
    if (is.data.frame(x$cmap) || is.matrix(x$cmap)) {
      color_r <- as.integer(x$cmap[[1]])
      color_g <- as.integer(x$cmap[[2]])
      color_b <- as.integer(x$cmap[[3]])
    }
  }

  # Build base tibble
  meta <- tibble::tibble(
    id = x$ids,
    label = x$labels,
    label_full = if (!is.null(x$orig_labels)) x$orig_labels else x$labels,
    hemi = x$hemi,
    color_r = color_r,
    color_g = color_g,
    color_b = color_b
  )

  # Add network if present (e.g., Schaefer atlas)
  if (!is.null(x$network) && length(x$network) == n) {
    meta$network <- x$network
  }

  meta
}


#' List Available ROI Attributes
#'
#' @description
#' Returns the names of attributes available for ROIs in an atlas. This is useful
#' for discovering what metadata is available for filtering or analysis.
#'
#' @param x An atlas object
#' @param ... Additional arguments passed to methods
#'
#' @return A character vector of attribute names that contain meaningful (non-NA)
#'   values. Excludes internal fields like color values and the id column.
#'
#' @examples
#' \dontrun{
#' # Discover available attributes
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' roi_attributes(atlas)
#' #> [1] "label" "label_full" "hemi" "network"
#'
#' # Compare with ASEG atlas (no network attribute)
#' aseg <- get_aseg_atlas()
#' roi_attributes(aseg)
#' #> [1] "label" "label_full" "hemi"
#' }
#'
#' @seealso \code{\link{roi_metadata}} for getting the full metadata tibble
#'
#' @export
roi_attributes <- function(x, ...) {
  UseMethod("roi_attributes")
}

#' @rdname roi_attributes
#' @export
roi_attributes.atlas <- function(x, ...) {
  meta <- roi_metadata(x)

  # Exclude id and color columns from attribute list
  internal_cols <- c("id", "color_r", "color_g", "color_b")
  cols <- setdiff(names(meta), internal_cols)

  # Filter out columns that are entirely NA
  has_values <- vapply(cols, function(col) !all(is.na(meta[[col]])), logical(1))
  cols[has_values]
}


#' Filter Atlas by ROI Attributes
#'
#' @description
#' Subsets an atlas object to include only ROIs matching specified criteria.
#' Uses non-standard evaluation (NSE) similar to \code{dplyr::filter()}.
#'
#' @param x An atlas object
#' @param ... Filter expressions using column names from \code{roi_metadata(x)}.
#'   Multiple conditions are combined with AND.
#' @param .dots A list of quosures for standard evaluation (advanced use)
#'
#' @return A new atlas object of the same class containing only the matching ROIs.
#'   The returned atlas has updated \code{ids}, \code{labels}, \code{hemi},
#'   \code{orig_labels}, \code{cmap}, \code{network} (if present), and
#'   \code{roi_metadata} fields. The underlying volume/surface data is also subset.
#'
#' @details
#' The filter operation creates a new atlas containing only the specified ROIs.
#' For volume atlases, voxels belonging to excluded ROIs are set to zero.
#' ROI IDs are preserved (not renumbered) to maintain consistency with the
#' original atlas labeling.
#'
#' @examples
#' \dontrun{
#' # Filter Schaefer atlas to left hemisphere visual network
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' left_vis <- filter_atlas(atlas, hemi == "left", network == "Vis")
#'
#' # Filter ASEG to left hemisphere structures
#' aseg <- get_aseg_atlas()
#' left_aseg <- filter_atlas(aseg, hemi == "left")
#'
#' # Filter by label pattern
#' hippo <- filter_atlas(aseg, grepl("Hippocampus", label))
#' }
#'
#' @seealso \code{\link{roi_metadata}} for viewing available filter columns,
#'   \code{\link{roi_attributes}} for listing available attributes,
#'   \code{\link{get_roi}} for extracting ROI data
#'
#' @importFrom rlang enquos eval_tidy
#' @export
filter_atlas <- function(x, ..., .dots = NULL) {
  UseMethod("filter_atlas")
}

#' @rdname filter_atlas
#' @export
filter_atlas.atlas <- function(x, ..., .dots = NULL) {
  meta <- roi_metadata(x)

  # Capture filter expressions
  dots <- if (!is.null(.dots)) .dots else rlang::enquos(...)

  # Apply filters sequentially
  mask <- rep(TRUE, nrow(meta))
  for (dot in dots) {
    result <- rlang::eval_tidy(dot, data = meta)
    if (!is.logical(result)) {
      stop("Filter expressions must evaluate to logical vectors")
    }
    # Treat NA as FALSE for filtering purposes
    result[is.na(result)] <- FALSE
    mask <- mask & result
  }

  # Get IDs to keep (use which() to avoid NA issues)
  keep_idx <- which(mask)

  if (length(keep_idx) == 0) {
    stop("Filter resulted in no matching ROIs")
  }

  keep_ids <- meta$id[keep_idx]

  # Subset the atlas
  .subset_atlas(x, keep_ids)
}


#' Subset Atlas to Specific ROI IDs (Internal)
#'
#' @param x An atlas object
#' @param keep_ids Numeric vector of ROI IDs to keep
#' @return A new atlas object with only the specified ROIs
#' @keywords internal
#' @noRd
.subset_atlas <- function(x, keep_ids) {
  # Find indices of ROIs to keep
  keep_idx <- which(x$ids %in% keep_ids)

  # Subset metadata fields
  new_ids <- x$ids[keep_idx]
  new_labels <- x$labels[keep_idx]
  new_hemi <- x$hemi[keep_idx]
  new_orig_labels <- if (!is.null(x$orig_labels)) x$orig_labels[keep_idx] else new_labels
  new_cmap <- if (!is.null(x$cmap)) x$cmap[keep_idx, , drop = FALSE] else NULL
  new_network <- if (!is.null(x$network)) x$network[keep_idx] else NULL

  # Subset the volume/surface
  new_atlas <- .subset_atlas_data(x$atlas, keep_ids)

  # Build new atlas object
  ret <- list(
    name = x$name,
    atlas = new_atlas,
    cmap = new_cmap,
    ids = new_ids,
    labels = new_labels,
    orig_labels = new_orig_labels,
    hemi = new_hemi
  )

  # Add network if present
  if (!is.null(new_network)) {
    ret$network <- new_network
  }

  # Build roi_metadata tibble
  ret$roi_metadata <- .build_roi_metadata(ret)

  # Preserve original class
  class(ret) <- class(x)
  ret
}


#' Subset Atlas Volume/Surface Data (Internal)
#'
#' @param atlas_data A NeuroVol, ClusteredNeuroVol, or surface atlas
#' @param keep_ids Numeric vector of ROI IDs to keep
#' @return Subset volume/surface with non-matching voxels set to zero
#' @keywords internal
#' @noRd
.subset_atlas_data <- function(atlas_data, keep_ids) {
  if (methods::is(atlas_data, "ClusteredNeuroVol")) {
    # For ClusteredNeuroVol, filter the clusters
    # @clusters contains one value per TRUE voxel in @mask
    old_clusters <- atlas_data@clusters
    keep_mask <- old_clusters %in% keep_ids

    if (!any(keep_mask)) {
      stop("No voxels remain after filtering")
    }

    # Get the original mask array and indices of TRUE voxels
    old_mask_arr <- methods::as(atlas_data@mask, "array")
    mask_indices <- which(old_mask_arr)

    # Create new mask: only keep voxels whose cluster is in keep_ids
    new_mask_arr <- array(FALSE, dim = dim(old_mask_arr))
    new_mask_arr[mask_indices[keep_mask]] <- TRUE

    # Create new LogicalNeuroVol mask
    mask_vol <- neuroim2::LogicalNeuroVol(new_mask_arr, neuroim2::space(atlas_data))

    # Filter clusters to only those being kept
    new_clusters <- old_clusters[keep_mask]

    neuroim2::ClusteredNeuroVol(mask = mask_vol, clusters = new_clusters)

  } else if (methods::is(atlas_data, "NeuroVol")) {
    # For regular NeuroVol, zero out non-matching voxels
    arr <- methods::as(atlas_data, "array")
    arr[!(arr %in% keep_ids)] <- 0L
    neuroim2::NeuroVol(arr, neuroim2::space(atlas_data))

  } else {
    # For other types (e.g., surface), return as-is with warning
    warning("Atlas data subsetting not fully supported for class: ", class(atlas_data)[1])
    atlas_data
  }
}


#' Build ROI Metadata Tibble (Internal)
#'
#' @param x An atlas object (list with ids, labels, etc.)
#' @return A tibble with ROI metadata
#' @keywords internal
#' @noRd
.build_roi_metadata <- function(x) {
  n <- length(x$ids)

  # Handle color map
  color_r <- color_g <- color_b <- rep(NA_integer_, n)
  if (!is.null(x$cmap) && nrow(x$cmap) == n) {
    color_r <- as.integer(x$cmap[[1]])
    color_g <- as.integer(x$cmap[[2]])
    color_b <- as.integer(x$cmap[[3]])
  }

  meta <- tibble::tibble(
    id = x$ids,
    label = x$labels,
    label_full = if (!is.null(x$orig_labels)) x$orig_labels else x$labels,
    hemi = x$hemi,
    color_r = color_r,
    color_g = color_g,
    color_b = color_b
  )

  if (!is.null(x$network)) {
    meta$network <- x$network
  }

  meta
}

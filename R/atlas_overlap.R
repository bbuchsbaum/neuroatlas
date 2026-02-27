#' Cross-Atlas Overlap Analysis
#'
#' @description
#' Computes spatial overlap between regions of two atlases using Dice and/or
#' Jaccard similarity coefficients. This is useful for comparing parcellations,
#' assessing correspondence between atlases, or mapping regions from one atlas
#' to another.
#'
#' @param atlas1 An atlas object (with \code{$atlas}, \code{$ids}, \code{$labels}).
#' @param atlas2 An atlas object (with \code{$atlas}, \code{$ids}, \code{$labels}).
#' @param metrics Character vector of overlap metrics to compute. One or more of
#'   \code{"dice"} and \code{"jaccard"}. Default is both.
#' @param min_overlap Integer. Minimum number of overlapping voxels for a pair
#'   to be included in the results. Default is 0 (include all pairs with any
#'   overlap).
#'
#' @return A \code{tibble} with columns:
#'   \describe{
#'     \item{atlas1_id}{Integer region ID from \code{atlas1}}
#'     \item{atlas1_label}{Character label from \code{atlas1}}
#'     \item{atlas2_id}{Integer region ID from \code{atlas2}}
#'     \item{atlas2_label}{Character label from \code{atlas2}}
#'     \item{dice}{Dice similarity coefficient (if requested)}
#'     \item{jaccard}{Jaccard similarity coefficient (if requested)}
#'     \item{n_overlap}{Number of overlapping voxels}
#'     \item{n_atlas1}{Total voxels in the \code{atlas1} region}
#'     \item{n_atlas2}{Total voxels in the \code{atlas2} region}
#'   }
#'   Rows are sorted by Dice coefficient descending (or Jaccard if Dice was not
#'   requested).
#'
#' @examples
#' \dontrun{
#' atlas_a <- get_schaefer_atlas(parcels = "100", networks = "7")
#' atlas_b <- get_schaefer_atlas(parcels = "200", networks = "7")
#' overlap <- atlas_overlap(atlas_a, atlas_b)
#' head(overlap)
#' }
#'
#' @importFrom methods is
#' @importFrom tibble tibble
#' @export
atlas_overlap <- function(atlas1, atlas2,
                          metrics = c("dice", "jaccard"),
                          min_overlap = 0) {
  metrics <- match.arg(metrics, c("dice", "jaccard"), several.ok = TRUE)
  min_overlap <- as.integer(min_overlap)

  vol1 <- .get_atlas_volume(atlas1)
  vol2 <- .get_atlas_volume(atlas2)

  # Check spatial dimensions match
  dims1 <- dim(vol1)[1:3]
  dims2 <- dim(vol2)[1:3]
  if (!all(dims1 == dims2)) {
    # Try resampling atlas2 into atlas1's space
    if (requireNamespace("neuroim2", quietly = TRUE) &&
        !is.null(tryCatch(neuroim2::space(vol1), error = function(e) NULL)) &&
        !is.null(tryCatch(neuroim2::space(vol2), error = function(e) NULL))) {
      vol2 <- tryCatch(
        neuroim2::resample(vol2, neuroim2::space(vol1)),
        error = function(e) NULL
      )
    } else {
      vol2 <- NULL
    }
    if (is.null(vol2)) {
      stop("Atlas dimensions do not match (",
           paste(dims1, collapse = "x"), " vs ",
           paste(dims2, collapse = "x"),
           ") and resampling failed.")
    }
  }

  # Densify to integer arrays
  arr1 <- .densify_atlas_vol(vol1)
  arr2 <- .densify_atlas_vol(vol2)

  # Count voxels per region in each atlas
  tab1 <- table(arr1[arr1 != 0L])
  tab2 <- table(arr2[arr2 != 0L])
  n1_map <- as.integer(tab1)
  names(n1_map) <- names(tab1)
  n2_map <- as.integer(tab2)
  names(n2_map) <- names(tab2)

 # Co-occurrence counting (vectorised)
  mask <- arr1 != 0L & arr2 != 0L
  if (!any(mask)) {
    # No overlap at all: return empty tibble
    cols <- list(
      atlas1_id = integer(0),
      atlas1_label = character(0),
      atlas2_id = integer(0),
      atlas2_label = character(0)
    )
    if ("dice" %in% metrics) cols$dice <- numeric(0)
    if ("jaccard" %in% metrics) cols$jaccard <- numeric(0)
    cols$n_overlap <- integer(0)
    cols$n_atlas1 <- integer(0)
    cols$n_atlas2 <- integer(0)
    return(tibble::as_tibble(cols))
  }

  v1 <- arr1[mask]
  v2 <- arr2[mask]
  pairs <- paste0(v1, "_", v2)
  counts <- table(pairs)

  # Parse pairs back to IDs
  pair_names <- names(counts)
  split_pairs <- strsplit(pair_names, "_", fixed = TRUE)
  id1 <- as.integer(vapply(split_pairs, `[`, character(1), 1L))
  id2 <- as.integer(vapply(split_pairs, `[`, character(1), 2L))
  n_overlap <- as.integer(counts)

  # Region sizes
  n_atlas1 <- n1_map[as.character(id1)]
  n_atlas2 <- n2_map[as.character(id2)]

  # Build label lookup
  label1_map <- stats::setNames(as.character(atlas1$labels), as.character(atlas1$ids))
  label2_map <- stats::setNames(as.character(atlas2$labels), as.character(atlas2$ids))
  atlas1_label <- unname(label1_map[as.character(id1)])
  atlas2_label <- unname(label2_map[as.character(id2)])

  # Compute metrics
  result <- tibble::tibble(
    atlas1_id = id1,
    atlas1_label = atlas1_label,
    atlas2_id = id2,
    atlas2_label = atlas2_label
  )

  if ("dice" %in% metrics) {
    result$dice <- 2 * n_overlap / (as.numeric(n_atlas1) + as.numeric(n_atlas2))
  }
  if ("jaccard" %in% metrics) {
    result$jaccard <- n_overlap / (as.numeric(n_atlas1) + as.numeric(n_atlas2) - n_overlap)
  }

  result$n_overlap <- as.integer(n_overlap)
  result$n_atlas1 <- as.integer(n_atlas1)
  result$n_atlas2 <- as.integer(n_atlas2)

  # Filter by min_overlap
  if (min_overlap > 0L) {
    result <- result[result$n_overlap >= min_overlap, , drop = FALSE]
  }

  # Sort by dice descending (or jaccard if dice not computed)
  if ("dice" %in% metrics) {
    result <- result[order(result$dice, decreasing = TRUE), , drop = FALSE]
  } else {
    result <- result[order(result$jaccard, decreasing = TRUE), , drop = FALSE]
  }

  result
}

#' Densify an atlas volume to an integer array
#'
#' Converts a \code{ClusteredNeuroVol} or \code{NeuroVol} to a plain integer
#' array with region IDs. Background voxels are 0.
#'
#' @param vol A \code{NeuroVol} or \code{ClusteredNeuroVol} object.
#' @return An integer array with dimensions matching the input volume.
#' @keywords internal
#' @noRd
.densify_atlas_vol <- function(vol) {
  if (methods::is(vol, "ClusteredNeuroVol")) {
    arr <- array(0L, dim = dim(vol))
    arr[which(vol@mask)] <- as.integer(vol@clusters)
    arr
  } else {
    arr <- as.array(vol)
    dim(arr) <- dim(vol)[1:3]
    storage.mode(arr) <- "integer"
    arr
  }
}

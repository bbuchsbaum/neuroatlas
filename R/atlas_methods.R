#' Default atlas mapping method
#'
#' Provides a fallback implementation of `map_atlas` for atlas objects.
#' Specific atlas classes can override this method if custom handling is
#' required.
#'
#' @param x An atlas object
#' @param vals Numeric vector of values to map
#' @param thresh Numeric vector of length two specifying a lower and upper
#'   threshold. Values outside this range are replaced with `NA`.
#' @param pos Logical. If `TRUE`, values are thresholded using their raw values;
#'   otherwise absolute values are used.
#' @param ... Additional arguments passed to methods
#'
#' @return A data frame with mapped statistics and region information or a
#'   `ggseg` atlas object when supported.
#' @export
map_atlas.atlas <- function(x, vals, thresh = c(0, 0), pos = FALSE, ...) {
  if (inherits(x, "schaefer")) {
    return(map_to_schaefer(x, vals, thresh = thresh, pos = pos))
  }

  stopifnot(length(vals) == length(x$orig_labels))
  fun <- if (pos) identity else abs
  tibble::tibble(
    statistic = ifelse(fun(vals) <= thresh[1] | fun(vals) > thresh[2], NA, vals),
    region = x$labels,
    label = x$orig_labels,
    hemi = x$hemi
  )
}

#' Default plot method for atlas objects
#'
#' Provides a simple plot wrapper that dispatches to atlas specific plotting
#' functions when available.
#'
#' @param x An atlas object
#' @param y Unused
#' @param ... Additional arguments passed to plotting functions
#' @export
plot.atlas <- function(x, y, ...) {
  if (inherits(x, "schaefer")) {
    return(ggseg_schaefer(x, ...))
  }
  stop("plot method not implemented for objects of class '", class(x)[1], "'")
}

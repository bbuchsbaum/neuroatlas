#' @rdname map_atlas
#' @param pos Logical. If `TRUE`, values are thresholded using raw values;
#'   otherwise the absolute values are used.
#' @export
map_atlas.atlas <- function(x, vals, thresh = c(0, 0), pos = FALSE, ...) {
  if (inherits(x, "schaefer")) {
    return(map_to_schaefer(x, vals, thresh = thresh, pos = pos))
  }

  stopifnot(length(vals) == length(x$orig_labels))
  fun <- if (pos) identity else abs
  
  # Handle different atlas structures
  result <- tibble::tibble(
    statistic = ifelse(fun(vals) <= thresh[1] | fun(vals) > thresh[2], NA, vals),
    label = x$orig_labels
  )
  
  # Add optional columns if they exist
  if (!is.null(x$labels)) {
    result$region <- x$labels
  }
  if (!is.null(x$hemi)) {
    result$hemi <- x$hemi
  }
  
  result
}

#' @rdname plot-methods
#' @export
plot.atlas <- function(x, y, ...) {
  if (inherits(x, "schaefer")) {
    return(ggseg_schaefer(x, ...))
  }
  stop("plot method not implemented for objects of class '", class(x)[1], "'")
}

#' @rdname map_atlas-methods
#' @inheritParams map_atlas
#' @param pos Logical. If `TRUE`, values are thresholded using raw values;
#'   otherwise the absolute values are used.
#' @describeIn map_atlas Default mapping for generic atlas objects.
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

#' @rdname plot-methods
#' @describeIn plot Plot method for generic atlas objects.
#' @export
plot.atlas <- function(x, y, ...) {
  if (inherits(x, "schaefer")) {
    return(ggseg_schaefer(x, ...))
  }
  stop("plot method not implemented for objects of class '", class(x)[1], "'")
}

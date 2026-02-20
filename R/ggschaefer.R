# Global variables to avoid R CMD check NOTEs
utils::globalVariables(c("hemi", "orig_label", "hemi.x", "hemi.y", "x", "y", "group", "fill", "region", "label"))

#' Get ggseg-Compatible Schaefer Atlas
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated in favour of the native
#' \code{\link{plot_brain}()} renderer.
#' Use \code{schaefer_surf()} to obtain a surface atlas and
#' \code{plot_brain()} for visualisation.
#'
#' @param atlas An atlas object containing Schaefer parcellation information.
#'
#' @return A ggseg brain atlas object for visualization (if ggsegSchaefer is
#'   installed).
#'
#' @examples
#' \donttest{
#' # Deprecated — use plot_brain(schaefer_surf(200, 17)) instead
#' }
#'
#' @importFrom stringr str_extract_all
#' @export
get_ggseg_atlas <- function(atlas) {
  lifecycle::deprecate_stop(
    "0.2.0", "get_ggseg_atlas()",
    "plot_brain()",
    details = "Use schaefer_surf() + plot_brain() for surface visualisation."
  )
}

#' @rdname map_atlas
#' @export
map_atlas.schaefer <- function(x, vals, thresh = NULL, pos = FALSE, ...) {
  # Delegate to the base atlas method (no ggseg dependency)
  map_atlas.atlas(x, vals, thresh = thresh, pos = pos, ...)
}

#' Map Values to Schaefer Atlas Format
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated. Use \code{\link{map_atlas}()} directly
#' or \code{\link{plot_brain}()} for visualisation.
#'
#' @param atlas An atlas object containing Schaefer parcellation information
#' @param vals Numeric vector of values to map to atlas regions
#' @param thresh Numeric vector of length 2 specifying (min, max) thresholds.
#' @param pos Logical. If TRUE, uses raw values; if FALSE, uses absolute values
#'   for thresholding. Default: FALSE
#'
#' @return A tibble with mapped values.
#'
#' @examples
#' \donttest{
#' # Deprecated — use map_atlas() or plot_brain() instead
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate left_join filter .data select coalesce
#' @importFrom tibble as_tibble
#' @export
map_to_schaefer <- function(atlas, vals, thresh = NULL, pos = FALSE) {
  lifecycle::deprecate_warn(
    "0.2.0", "map_to_schaefer()",
    "map_atlas()",
    details = "map_atlas() now works for all atlas types including Schaefer."
  )

  # Fall back to the base map_atlas implementation
  map_atlas.atlas(atlas, vals, thresh = thresh, pos = pos)
}

#' Create Interactive Schaefer Atlas Visualization
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated in favour of \code{\link{plot_brain}()}.
#' Use \code{plot_brain(schaefer_surf(...))} for interactive cortical surface
#' visualisation.
#'
#' @param atlas An atlas object containing Schaefer parcellation information
#' @param vals Numeric vector of values to visualize on the atlas
#' @param thresh Numeric vector of length 2 specifying (min, max) thresholds.
#' @param pos Logical. If TRUE, uses raw values for thresholding.
#' @param palette Character string specifying the color palette. Default: "Spectral"
#' @param interactive Logical. If TRUE, creates an interactive plot. Default: TRUE
#' @param lim Numeric vector of length 2 specifying the range for color mapping.
#'
#' @return A ggplot2 or ggiraph object.
#'
#' @examples
#' \dontrun{
#' # Deprecated — use plot_brain(schaefer_surf(200, 17), vals = ...) instead
#' }
#'
#' @importFrom ggplot2 aes scale_fill_distiller ggplot theme_void coord_fixed
#' @importFrom ggiraph girafe opts_tooltip opts_hover opts_selection geom_polygon_interactive
#' @importFrom scales squish
#' @export
ggseg_schaefer <- function(atlas, vals, thresh = NULL, pos = FALSE,
                           palette = "Spectral", interactive = TRUE,
                           lim = range(vals)) {
  lifecycle::deprecate_stop(
    "0.2.0", "ggseg_schaefer()",
    "plot_brain()",
    details = "Use plot_brain(schaefer_surf(...), vals = ...) instead."
  )
}

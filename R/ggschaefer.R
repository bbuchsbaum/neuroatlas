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
  lifecycle::deprecate_warn(
    "0.2.0", "get_ggseg_atlas()",
    "plot_brain()",
    details = "Use schaefer_surf() + plot_brain() for surface visualisation."
  )

  if (!requireNamespace("ggsegSchaefer", quietly = TRUE)) {
    stop("Package 'ggsegSchaefer' is required for this deprecated function but is not installed.\n",
         "Consider using plot_brain(schaefer_surf(...)) instead.",
         call. = FALSE)
  }

  matches <- stringr::str_extract_all(atlas$name, "\\d+")
  atlas_num <- unlist(matches)
  parcels <- as.numeric(atlas_num[1])
  networks <- as.numeric(atlas_num[2])

  if ((parcels %in% seq(100, 1000, 100)) && (networks %in% c(7, 17))) {
    atlas_string <- paste0("schaefer", networks, "_", parcels)
    atlas_obj <- tryCatch({
      if (exists(atlas_string, envir = asNamespace("ggsegSchaefer"))) {
        get(atlas_string, envir = asNamespace("ggsegSchaefer"))
      } else {
        eval(parse(text = paste0("ggsegSchaefer::", atlas_string)))
      }
    }, error = function(e) {
      stop("Failed to load atlas '", atlas_string, "' from ggsegSchaefer: ", e$message,
           call. = FALSE)
    })
    return(atlas_obj)
  } else {
    stop("Invalid atlas name. Must be Schaefer atlas with 7 or 17 networks and 100-1000 parcels.")
  }
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
  lifecycle::deprecate_warn(
    "0.2.0", "ggseg_schaefer()",
    "plot_brain()",
    details = "Use plot_brain(schaefer_surf(...), vals = ...) instead."
  )

  if (!requireNamespace("ggseg", quietly = TRUE) ||
      !requireNamespace("ggsegSchaefer", quietly = TRUE)) {
    stop("Packages 'ggseg' and 'ggsegSchaefer' are required for this deprecated function.\n",
         "Consider using plot_brain(schaefer_surf(...), vals = ...) instead.",
         call. = FALSE)
  }

  mapped_data <- map_atlas.atlas(atlas, vals, thresh = thresh, pos = pos)

  # Attempt legacy ggseg path if packages are available
  gatl <- get_ggseg_atlas(atlas)
  gatl$data <- mapped_data

  ggseg_fn <- get("ggseg", envir = asNamespace("ggseg"))
  ggobj <- ggseg_fn(
    atlas = gatl,
    position = "stacked",
    colour = "gray",
    guide = TRUE,
    mapping = aes(fill = .data$statistic)
  )

  ggobj <- ggobj +
    scale_fill_distiller(
      palette = palette,
      limits = lim,
      direction = -1,
      oob = scales::squish
    )

  if (interactive) {
    ggiraph::girafe(
      ggobj = ggobj,
      width_svg = 8,
      height_svg = 6,
      options = list(
        opts_tooltip(opacity = .7,
                     css = "font-family: Arial, Helvetica, sans-serif;"),
        opts_hover(css = "fill:yellow;"),
        opts_selection(css = "fill:red;",
                       type = "single",
                       only_shiny = FALSE)
      )
    )
  } else {
    ggobj
  }
}

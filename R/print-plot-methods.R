#' Print Methods for neuroatlas Objects
#'
#' @description
#' Print methods for various atlas objects in the neuroatlas package
#'
#' @param x An atlas object (atlas, glasser, schaefer, etc.)
#' @param ... Additional arguments passed to print
#'
#' @return The object is returned invisibly
#'
#' @examples
#' atlas <- get_aseg_atlas()
#' print(atlas)
#'
#' @name print-methods
NULL

#' Plot Methods for neuroatlas Objects
#'
#' @description
#' Volumetric slice visualisation for any atlas object. By default renders a
#' multi-slice montage with each parcel coloured by the
#' \code{\link{atlas_roi_colors}()} system. An orthogonal three-plane view is
#' also available.
#'
#' @param x An atlas object (atlas, glasser, schaefer, etc.)
#' @param y Ignored (required for S3 consistency)
#' @param view Character; \code{"montage"} (default) for a multi-slice montage
#'   or \code{"ortho"} for three orthogonal planes.
#' @param method Colour algorithm passed to \code{\link{atlas_roi_colors}()}.
#'   One of \code{"rule_hcl"} (default), \code{"network_harmony"},
#'   \code{"maximin_view"}, or \code{"embedding"}.
#' @param colors Optional pre-computed colour specification: a tibble from
#'   \code{\link{atlas_roi_colors}()}, or a named character vector of hex
#'   colours keyed by region ID.
#' @param nslices Number of slices for montage view (default 12).
#' @param ... Additional arguments passed to \code{neuroim2::plot_montage()} or
#'   \code{neuroim2::plot_ortho()}.
#'
#' @return For \code{view = "montage"}, a \code{ggplot2} object (invisibly).
#'   For \code{view = "ortho"}, a \code{patchwork} composite (if available) or a
#'   list of three \code{ggplot2} objects.
#'
#' @examples
#' \donttest{
#' atlas <- get_aseg_atlas()
#' plot(atlas)
#' plot(atlas, view = "ortho")
#' plot(atlas, method = "maximin_view", nslices = 6)
#' }
#'
#' @seealso \code{\link{atlas_roi_colors}}, \code{\link{ggseg_schaefer}}
#'
#' @name plot-methods
NULL

#' @rdname print-methods
#' @importFrom cli rule symbol
#' @importFrom crayon bold blue green yellow red white
#' @export
print.schaefer <- function(x, ...) {
  cat(cli::rule(crayon::bold("Schaefer Atlas"), line = 2), "\n")
  cat(crayon::blue("Name:"), x$name, "\n")
  
  if (!is.null(x$atlas)) {
    dims <- dim(x$atlas)
    cat(crayon::blue("Dimensions:"), paste(dims, collapse = " x "), "\n")
  }
  
  n_regions <- length(x$ids)
  cat(crayon::blue("Regions:"), n_regions, "\n")
  
  # Extract network count from name
  if (grepl("\\d+networks", x$name)) {
    networks <- sub(".*-(\\d+)networks.*", "\\1", x$name)
    cat(crayon::blue("Networks:"), networks, "\n")
  }
  
  # Hemisphere distribution
  if (!is.null(x$hemi)) {
    hemi_table <- table(x$hemi)
    hemi_str <- paste(names(hemi_table), hemi_table, sep = ": ", collapse = ", ")
    cat(crayon::blue("Hemispheres:"), hemi_str, "\n")
  }
  
  # Network distribution if available
  if (!is.null(x$network)) {
    unique_networks <- length(unique(x$network))
    cat(crayon::blue("Unique networks:"), unique_networks, "\n")
  }
  
  invisible(x)
}

#' @rdname plot-methods
#' @details
#' \code{plot.surfatlas} renders a cortical surface projection via
#' \code{\link{plot_brain}()}.
#' @param views Character vector of views to render for surface atlases.
#'   Any combination of \code{"lateral"}, \code{"medial"}, \code{"dorsal"},
#'   \code{"ventral"}. Default: \code{c("lateral", "medial")}.
#' @export
plot.surfatlas <- function(x, y, vals = NULL,
                           views = c("lateral", "medial"),
                           ...) {
  plot_brain(x, vals = vals, views = views, ...)
}
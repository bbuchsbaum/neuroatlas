#' Multi-panel Brain Plot Grid
#'
#' Arranges multiple brain surface plots into a grid layout using
#' \pkg{patchwork}. Each element of \code{vals_list} produces one panel
#' rendered by \code{\link{plot_brain}}.
#'
#' @param surfatlas A \code{surfatlas} object (e.g., from
#'   \code{\link{schaefer_surf}} or \code{\link{glasser_surf}}).
#' @param vals_list A named list of numeric vectors, one per panel. Each
#'   vector must have length equal to the number of atlas regions.
#' @param views Character vector of views passed to \code{\link{plot_brain}}.
#'   Default: \code{c("lateral", "medial")}.
#' @param hemis Character vector of hemispheres passed to
#'   \code{\link{plot_brain}}. Default: \code{c("left", "right")}.
#' @param ncol Integer number of columns in the grid layout. If \code{NULL},
#'   chosen automatically.
#' @param shared_scale Logical. If \code{TRUE} (default), all panels share
#'   the same colour scale computed from the range of all values.
#' @param palette Character: scico palette name. Default: \code{"cork"}.
#' @param lim Optional numeric length-2 colour limits. Overrides automatic
#'   limits when provided.
#' @param titles Optional character vector of panel titles. If \code{NULL},
#'   names of \code{vals_list} are used.
#' @param colorbar Logical. If \code{TRUE} (default), a standalone colorbar
#'   panel is appended to the grid.
#' @param ... Additional arguments passed to \code{\link{plot_brain}}.
#'
#' @return A \code{patchwork} object.
#'
#' @examples
#' \dontrun{
#' atl <- schaefer_surf(200, 7)
#' vals <- list(
#'   Contrast_A = rnorm(200),
#'   Contrast_B = rnorm(200)
#' )
#' plot_brain_grid(atl, vals)
#' }
#'
#' @export
plot_brain_grid <- function(surfatlas,
                            vals_list,
                            views = c("lateral", "medial"),
                            hemis = c("left", "right"),
                            ncol = NULL,
                            shared_scale = TRUE,
                            palette = "cork",
                            lim = NULL,
                            titles = NULL,
                            colorbar = TRUE,
                            ...) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for plot_brain_grid(). ",
         "Install it with install.packages('patchwork').", call. = FALSE)
  }

  if (!is.list(vals_list) || length(vals_list) == 0) {
    stop("'vals_list' must be a non-empty named list of numeric vectors.",
         call. = FALSE)
  }

  n_panels <- length(vals_list)

  if (is.null(titles)) {
    titles <- names(vals_list)
    if (is.null(titles)) titles <- paste("Panel", seq_len(n_panels))
  }

  # Compute shared limits
  if (shared_scale && is.null(lim)) {
    lim <- range(unlist(vals_list), na.rm = TRUE)
  }

  # Build individual panels
  panels <- vector("list", n_panels)
  for (i in seq_len(n_panels)) {
    panel_lim <- if (shared_scale) lim else NULL
    panels[[i]] <- plot_brain(
      surfatlas,
      vals = vals_list[[i]],
      views = views,
      hemis = hemis,
      palette = palette,
      lim = panel_lim,
      interactive = FALSE,
      colorbar = FALSE,
      ...
    ) + ggplot2::ggtitle(titles[i])
  }

  # Arrange panels
  combined <- patchwork::wrap_plots(panels, ncol = ncol)

  # Add colorbar
  if (isTRUE(colorbar)) {
    final_lim <- if (!is.null(lim)) {
      lim
    } else {
      range(unlist(vals_list), na.rm = TRUE)
    }
    cb <- .make_colorbar_panel(palette = palette, lim = final_lim)
    combined <- combined + cb +
      patchwork::plot_layout(widths = c(rep(1, n_panels), 0.15))
  }

  combined + patchwork::plot_annotation(
    theme = ggplot2::theme(plot.background = ggplot2::element_rect(
      fill = "white", colour = NA
    ))
  )
}

#' Multi-panel Brain Plot Grid
#'
#' Arranges multiple brain surface plots into a grid layout using
#' \pkg{patchwork}. Each element of \code{vals_list} produces one panel
#' rendered by \code{\link{plot_brain}}.
#'
#' @param surfatlas A \code{surfatlas} object (e.g., from
#'   \code{\link{schaefer_surf}} or \code{\link{glasser_surf}}).
#' @param vals_list A list of numeric vectors, one per panel. Each
#'   vector must have length equal to the number of atlas regions. Optional
#'   when \code{data} and \code{values} are supplied. Names, when present,
#'   become the default panel titles.
#' @param data Optional data frame, tibble, or \code{parcel_data} object with
#'   one row per parcel. Supply either \code{data} or \code{vals_list}.
#' @param values Character vector naming numeric columns in \code{data}; one
#'   brain map is produced for each column.
#' @param by Parcel-key specification passed to
#'   \code{\link{align_parcel_values}()}.
#' @param allow_partial Logical. If \code{FALSE} (default), \code{data} must
#'   contain every atlas parcel. If \code{TRUE}, missing parcels receive
#'   \code{NA} values.
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
#' @param colorbar Logical or character. Use \code{TRUE} or \code{"right"}
#'   (default) for a vertical shared colorbar, \code{"bottom"} for a
#'   horizontal shared colorbar, or \code{FALSE} / \code{"none"} to omit it.
#' @param colorbar_title Optional shared colorbar title.
#' @param title,subtitle,caption Optional overall plot annotations applied to
#'   the composed figure.
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
#'
#' results <- data.frame(
#'   id = rev(atl$ids),
#'   Contrast_A = rnorm(200),
#'   Contrast_B = rnorm(200)
#' )
#' plot_brain_grid(
#'   atl,
#'   data = results,
#'   values = c("Contrast_A", "Contrast_B")
#' )
#' }
#'
#' @export
plot_brain_grid <- function(surfatlas,
                            vals_list = NULL,
                            views = c("lateral", "medial"),
                            hemis = c("left", "right"),
                            ncol = NULL,
                            shared_scale = TRUE,
                            palette = "cork",
                            lim = NULL,
                            titles = NULL,
                            colorbar = TRUE,
                            colorbar_title = NULL,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            data = NULL,
                            values = NULL,
                            by = NULL,
                            allow_partial = FALSE,
                            ...) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for plot_brain_grid(). ",
         "Install it with install.packages('patchwork').", call. = FALSE)
  }
  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.", call. = FALSE)
  }
  if (!is.logical(shared_scale) || length(shared_scale) != 1L ||
      is.na(shared_scale)) {
    stop("'shared_scale' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(ncol) &&
      (!is.numeric(ncol) || length(ncol) != 1L || !is.finite(ncol) ||
       ncol < 1 || ncol != as.integer(ncol))) {
    stop("'ncol' must be NULL or a positive integer scalar.", call. = FALSE)
  }
  if (!is.null(ncol)) ncol <- as.integer(ncol)
  dots <- list(...)
  if ("static_backend" %in% names(dots) &&
      identical(match.arg(dots$static_backend, c("ggplot", "cpu")), "cpu")) {
    cli::cli_abort(
      c(
        "{.fn plot_brain_grid} does not support the CPU backend for parcel maps.",
        "i" = "Use {.code static_backend = 'ggplot'} (the default).",
        "i" = paste0(
          "Use {.fn plot_brain} with a vertex-wise {.arg overlay} for ",
          "CPU rendering."
        )
      ),
      class = c("neuroatlas_error_unsupported", "neuroatlas_error")
    )
  }

  data_present <- !is.null(data)
  if (data_present) {
    if (!is.null(vals_list)) {
      cli::cli_abort(
        "Supply either {.arg data} or {.arg vals_list}, not both.",
        class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
      )
    }
    if (!is.character(values) || length(values) == 0L || anyNA(values) ||
        any(!nzchar(values)) || anyDuplicated(values)) {
      cli::cli_abort(
        "{.arg values} must be a non-empty character vector of unique column names.",
        class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
      )
    }
    aligned <- .align_atlas_table(
      atlas = surfatlas,
      data = data,
      by = by,
      allow_partial = allow_partial
    )
    vals_list <- stats::setNames(
      lapply(values, function(value_col) {
        .parcel_value_from_aligned(aligned, surfatlas, value_col)
      }),
      values
    )
  } else if (!is.null(values) || !is.null(by) ||
             !identical(allow_partial, FALSE)) {
    cli::cli_abort(
      "{.arg values}, {.arg by}, and {.arg allow_partial} require {.arg data}.",
      class = c("neuroatlas_error_parcel_value", "neuroatlas_error")
    )
  }

  if (!is.list(vals_list) || length(vals_list) == 0) {
    stop("'vals_list' must be a non-empty list of numeric vectors.",
         call. = FALSE)
  }
  valid_map <- vapply(vals_list, function(x) {
    (is.numeric(x) || is.integer(x)) &&
      is.null(dim(x)) &&
      length(x) == length(surfatlas$ids) &&
      !any(is.infinite(x)) && any(is.finite(x))
  }, logical(1))
  if (any(!valid_map)) {
    stop("Each map must be a numeric atlas-length vector with at least one ",
         "finite value and no infinite values.", call. = FALSE)
  }
  colorbar_position <- .normalize_colorbar_position(colorbar)

  n_panels <- length(vals_list)

  if (is.null(titles)) {
    titles <- names(vals_list)
    if (is.null(titles) || length(titles) != n_panels ||
        any(!nzchar(titles))) {
      titles <- paste("Panel", seq_len(n_panels))
    }
  }
  if (!is.character(titles) || length(titles) != n_panels || anyNA(titles)) {
    stop("'titles' must be NULL or one character value per map.",
         call. = FALSE)
  }

  # Compute shared limits
  if (!is.null(lim) &&
      (!is.numeric(lim) || length(lim) != 2L || any(!is.finite(lim)) ||
       lim[[1]] > lim[[2]])) {
    stop("'lim' must contain two finite numeric values in increasing order.",
         call. = FALSE)
  }
  if (shared_scale && is.null(lim)) {
    all_vals <- unlist(vals_list, use.names = FALSE)
    lim <- range(all_vals[is.finite(all_vals)])
    if (lim[[1]] == lim[[2]]) {
      lim <- lim + c(-1, 1) * max(abs(lim[[1]]) * 1e-8, 1e-8)
    }
  }
  if (!shared_scale && is.null(lim) &&
      !identical(colorbar_position, "none")) {
    stop("A shared colorbar is not meaningful with independent panel scales. ",
         "Set 'colorbar = FALSE', 'shared_scale = TRUE', or provide 'lim'.",
         call. = FALSE)
  }

  # Build individual panels
  panels <- vector("list", n_panels)
  for (i in seq_len(n_panels)) {
    panel_lim <- if (shared_scale || !is.null(lim)) lim else NULL
    panels[[i]] <- plot_brain(
      surfatlas,
      vals = vals_list[[i]],
      views = views,
      hemis = hemis,
      palette = palette,
      lim = panel_lim,
      interactive = FALSE,
      colorbar = "none",
      title = titles[i],
      ...
    )
  }

  # Arrange panels
  combined <- patchwork::wrap_plots(panels, ncol = ncol)

  # Add colorbar
  cb <- NULL
  if (!identical(colorbar_position, "none")) {
    final_lim <- if (!is.null(lim)) {
      lim
    } else {
      all_vals <- unlist(vals_list, use.names = FALSE)
      range(all_vals[is.finite(all_vals)])
    }
    cb <- .make_colorbar_panel(
      palette = palette,
      lim = final_lim,
      title = colorbar_title,
      position = colorbar_position
    )
  }

  .compose_plot_brain_figure(
    main_plot = combined,
    colorbar_plot = cb,
    colorbar_position = colorbar_position,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}

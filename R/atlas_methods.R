#' @rdname map_atlas
#' @param pos Logical. If `TRUE`, values are thresholded using raw values;
#'   otherwise the absolute values are used.
#' @export
map_atlas.atlas <- function(x, vals, thresh = NULL, pos = FALSE, ...) {
  stopifnot(length(vals) == length(x$orig_labels))

  statistic <- vals

  # Apply optional thresholding
  if (!is.null(thresh)) {
    stopifnot(is.numeric(thresh), length(thresh) == 2)
    fun <- if (pos) identity else abs
    statistic <- ifelse(fun(vals) <= thresh[1] | fun(vals) > thresh[2],
                        NA, vals)
  }

  result <- tibble::tibble(
    statistic = statistic,
    label = x$orig_labels
  )

  if (!is.null(x$labels)) {
    result$region <- x$labels
  }
  if (!is.null(x$hemi)) {
    result$hemi <- x$hemi
  }

  result
}

#' @rdname plot-methods
#' @param view Character; \code{"montage"} (default) for a multi-slice montage
#'   or \code{"ortho"} for three orthogonal planes.
#' @param method Colour algorithm passed to \code{\link{atlas_roi_colors}()}.
#'   One of \code{"rule_hcl"}, \code{"network_harmony"}, \code{"maximin_view"},
#'   or \code{"embedding"}.
#' @param colors Optional pre-computed colour specification: either a tibble
#'   from \code{\link{atlas_roi_colors}()} (with \code{id} and \code{color}
#'   columns), or a named character vector of hex colours keyed by region ID.
#' @param nslices Number of slices for montage view (default 12).
#' @param legend Logical. If \code{TRUE}, draw a region colour legend
#'   (label -> colour) below the \code{"montage"} view. Default \code{FALSE}. A
#'   legend is only useful for small atlases, so it is suppressed (with a
#'   warning) when the atlas has more than \code{legend_max} regions; it is also
#'   omitted for \code{view = "ortho"} (each plane shows a different subset of
#'   regions). Labels occurring in both hemispheres are disambiguated with
#'   \code{(L)}/\code{(R)}.
#' @param legend_max Integer. Maximum number of regions for which a legend is
#'   drawn when \code{legend = TRUE}. Default \code{30}.
#' @importFrom ggplot2 scale_fill_manual
#' @export
plot.atlas <- function(x, y, view = c("montage", "ortho"),
                       method = "rule_hcl", colors = NULL,
                       nslices = 12L, legend = FALSE, legend_max = 30L, ...) {
  view <- match.arg(view)
  if (!is.logical(legend) || length(legend) != 1L || is.na(legend)) {
    stop("'legend' must be TRUE or FALSE.", call. = FALSE)
  }

  # A legend only helps for small atlases; cap it (with a warning when the user
  # explicitly asked) so a 400-region parcellation never draws one.
  n_reg <- length(x$ids)
  show_legend <- isTRUE(legend) && n_reg <= legend_max
  if (isTRUE(legend) && n_reg > legend_max) {
    cli::cli_warn(c(
      "Not drawing a legend for {n_reg} regions (> {.arg legend_max} = {legend_max}).",
      "i" = "Raise {.arg legend_max} to force it."
    ))
  }
  # Each orthographic plane shows a different subset of regions, so a per-panel
  # legend cannot be reconciled into one. The legend is drawn for the montage
  # view (whose slices together span the atlas).
  if (show_legend && view == "ortho") {
    cli::cli_inform(
      "A region legend is only drawn for {.code view = \"montage\"}; omitting it here."
    )
    show_legend <- FALSE
  }

 # --- Resolve colours ---
  if (is.null(colors)) {
    color_tbl <- atlas_roi_colors(x, method = method)
  } else if (is.data.frame(colors)) {
    color_tbl <- colors
  } else if (is.character(colors)) {
    # Named vector of hex colours
    if (!is.null(names(colors))) {
      color_tbl <- tibble::tibble(id = as.integer(names(colors)), color = unname(colors))
    } else {
      color_tbl <- tibble::tibble(id = x$ids, color = colors)
    }
  } else {
    stop("'colors' must be NULL, a tibble, or a character vector of hex colours")
  }

  # Build named colour map: character ID -> hex (include background)
  color_map <- stats::setNames(color_tbl$color, as.character(color_tbl$id))
  color_map["0"] <- "transparent"

  vol <- .get_atlas_volume(x)

  # Legend labels: region names, disambiguating any label that appears in both
  # hemispheres (e.g. "Thalamus" -> "Thalamus (L)" / "Thalamus (R)").
  leg_labels <- as.character(x$labels)
  if (show_legend && !is.null(x$hemi)) {
    dup <- leg_labels %in% leg_labels[duplicated(leg_labels)]
    suff <- c(left = " (L)", right = " (R)")[as.character(x$hemi)]
    suff[is.na(suff)] <- ""
    leg_labels[dup] <- paste0(leg_labels[dup], suff[dup])
  }
  fill_guide <- if (show_legend) {
    ggplot2::guide_legend(ncol = min(n_reg, 6L))
  } else {
    "none"
  }

  # Helper to apply discrete colour scale to a ggplot from plot_montage/ortho
  .apply_atlas_scale <- function(p) {
    # Convert the continuous 'value' column to factor for discrete scale
    p$data$value <- factor(as.character(round(p$data$value)),
                           levels = names(color_map))
    # Suppress "Scale for fill is already present" message from replacing
    # the continuous scale that plot_montage/plot_ortho sets
    suppressMessages(
      p <- p + ggplot2::scale_fill_manual(
        values = color_map,
        breaks = as.character(x$ids),
        labels = leg_labels,
        na.value = "transparent",
        name = NULL,
        drop = FALSE,        # keep all regions so the legend is complete and
                             # identical across ortho panels (collectable)
        guide = fill_guide
      ) + neuroim2::theme_neuro()
    )
    # theme_neuro() hides the legend; re-enable it (at the bottom, so it does
    # not crowd the slice grid) when requested. plot_montage/plot_ortho style
    # the key as a tall, thin colour*bar* (key.height = 3cm), so reset the key
    # dimensions for a discrete swatch legend.
    if (show_legend) {
      p <- p + ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 7),
        legend.key.height = ggplot2::unit(0.8, "lines"),
        legend.key.width = ggplot2::unit(0.8, "lines")
      )
    }
    p
  }

  if (view == "montage") {
    # Select evenly-spaced slices through the volume
    zdim <- dim(vol)[3]
    # Find slices that contain parcel voxels
    arr <- if (methods::is(vol, "ClusteredNeuroVol")) {
      a <- array(0L, dim = dim(vol))
      a[which(vol@mask)] <- vol@clusters
      a
    } else {
      vol[,,]
    }
    nonempty <- which(apply(arr, 3, function(sl) any(sl != 0)))
    if (length(nonempty) == 0) {
      stop("Atlas volume contains no non-zero voxels")
    }
    n_use <- min(nslices, length(nonempty))
    idx <- nonempty[round(seq(1, length(nonempty), length.out = n_use))]

    p <- neuroim2::plot_montage(vol, zlevels = idx, ncol = min(6L, n_use), ...)
    p <- .apply_atlas_scale(p)
    print(p)
    invisible(p)
  } else {
    # ortho: list of 3 ggplots
    plots <- neuroim2::plot_ortho(vol, ...)
    plots <- lapply(plots, .apply_atlas_scale)
    if (requireNamespace("patchwork", quietly = TRUE)) {
      # Collect the three identical panel legends into one shared legend.
      combined <- patchwork::wrap_plots(
        plots, ncol = 3,
        guides = if (show_legend) "collect" else "keep"
      )
      print(combined)
      invisible(combined)
    } else {
      for (pl in plots) print(pl)
      invisible(plots)
    }
  }
}

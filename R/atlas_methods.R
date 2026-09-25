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
    if (!is.data.frame(p$data) || !"value" %in% names(p$data)) {
      stop("Cannot recolour the neuroim2 slice plot: its data has no ",
           "'value' column (unsupported neuroim2 plot structure).",
           call. = FALSE)
    }
    p$data$value <- factor(.atlas_value_ids(p$data$value),
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

    p <- .call_neuroim2_plot(
      neuroim2::plot_montage, vol,
      args = list(zlevels = idx, ncol = min(6L, n_use)),
      optional = list(interpolate = FALSE),
      dots = list(...)
    )
    p <- .apply_atlas_scale(p)
    print(p)
    invisible(p)
  } else {
    # ortho: list of 3 ggplots
    # neuroim2 >= 0.19 returns one assembled patchwork figure by default;
    # ask for the per-plane ggplots (assemble = FALSE) so each can be
    # recoloured. Older versions return the list directly.
    plots <- .call_neuroim2_plot(
      neuroim2::plot_ortho, vol,
      optional = list(assemble = FALSE, interpolate = FALSE, crop = FALSE),
      dots = list(...)
    )
    if (inherits(plots, "ggplot")) {
      stop("neuroim2::plot_ortho() returned a single assembled figure; ",
           "cannot recolour its panels.", call. = FALSE)
    }
    plane_order <- c("sagittal", "coronal", "axial")
    if (all(plane_order %in% names(plots))) {
      plots <- plots[plane_order]
    }
    plots <- lapply(unname(plots), .apply_atlas_scale)
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

#' Convert slice-plot values to atlas region-ID strings
#'
#' neuroim2's slice plots store voxel values in a `value` column whose type
#' has varied across versions (numeric, and possibly factor or character).
#' Returns the rounded integer label as a character vector, `NA` for values
#' that are not interpretable as numbers.
#' @param value Vector of slice values.
#' @return Character vector of region IDs.
#' @keywords internal
#' @noRd
.atlas_value_ids <- function(value) {
  if (is.list(value)) {
    value <- vapply(value, function(v) {
      if (length(v) == 1L) as.character(v) else NA_character_
    }, character(1))
  }
  if (is.factor(value)) value <- as.character(value)
  if (!is.numeric(value)) {
    value <- suppressWarnings(as.numeric(as.character(value)))
  }
  out <- as.character(round(value))
  out[is.na(value)] <- NA_character_
  out
}

#' Call a neuroim2 slice-plot function with version-tolerant arguments
#'
#' `args` are always passed. `optional` arguments are passed only when the
#' installed neuroim2 function accepts them, so the same call works with
#' neuroim2 releases before and after the 0.19 plotting redesign. User
#' arguments in `dots` take precedence over both.
#' @keywords internal
#' @noRd
.call_neuroim2_plot <- function(fun, vol, args = list(), optional = list(),
                                dots = list()) {
  optional <- optional[names(optional) %in% names(formals(fun))]
  args <- utils::modifyList(utils::modifyList(args, optional), dots)
  do.call(fun, c(list(vol), args))
}

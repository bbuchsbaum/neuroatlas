# Panel layout helpers for plot_brain().
#
# Computes per-panel translation/scale transforms, applies them to
# polygon and segment data, normalises colorbar positioning, and
# resolves user-supplied panel-label overrides against the canonical
# panel levels. Pure coordinate bookkeeping -- no ggplot layers.
#
# Split out from plot_brain.R; behaviour is intentionally unchanged.

#' Compute panel-wise layout transforms for projected brain polygons
#'
#' @param poly_data Polygon data with columns \code{panel}, \code{view},
#'   \code{x}, \code{y}.
#' @param panel_layout Character scalar: \code{"native"} or
#'   \code{"presentation"}.
#' @return A tibble with per-panel transform parameters, or \code{NULL}.
#' @keywords internal
#' @noRd
.compute_panel_layout_transforms <- function(poly_data,
                                             panel_layout = c("native",
                                                              "presentation")) {
  panel_layout <- match.arg(panel_layout)
  if (panel_layout == "native") return(NULL)
  if (is.null(poly_data) || nrow(poly_data) == 0) return(NULL)

  needed <- c("panel", "view", "x", "y")
  if (!all(needed %in% names(poly_data))) return(NULL)

  ext <- poly_data |>
    dplyr::group_by(panel, view) |>
    dplyr::summarise(
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cx = (xmin + xmax) / 2,
      cy = (ymin + ymax) / 2,
      width = xmax - xmin,
      height = ymax - ymin,
      rotate = view %in% c("dorsal", "ventral"),
      width_rot = dplyr::if_else(rotate, height, width),
      height_rot = dplyr::if_else(rotate, width, height)
    )

  span <- pmax(ext$width_rot, ext$height_rot)
  span[!is.finite(span) | span <= 0] <- 1
  ext$scale <- 1 / span

  ext[, c("panel", "cx", "cy", "rotate", "scale")]
}

#' Apply panel-wise transforms to point geometry
#'
#' @param dat Data frame with columns \code{panel}, \code{x}, \code{y}.
#' @param transforms Output of [.compute_panel_layout_transforms()].
#' @param x_col Name of x column.
#' @param y_col Name of y column.
#' @return Transformed data frame.
#' @keywords internal
#' @noRd
.apply_panel_layout_to_points <- function(dat, transforms,
                                          x_col = "x", y_col = "y") {
  if (is.null(dat) || nrow(dat) == 0) return(dat)
  if (is.null(transforms) || nrow(transforms) == 0) return(dat)
  if (!all(c("panel", x_col, y_col) %in% names(dat))) return(dat)

  out <- dplyr::left_join(
    dat,
    transforms,
    by = "panel"
  )

  cx <- out$cx
  cy <- out$cy
  rotate <- out$rotate
  scale <- out$scale

  missing_t <- is.na(cx) | is.na(cy) | is.na(scale)
  cx[missing_t] <- 0
  cy[missing_t] <- 0
  rotate[is.na(rotate)] <- FALSE
  scale[missing_t] <- 1

  x0 <- out[[x_col]] - cx
  y0 <- out[[y_col]] - cy

  x1 <- ifelse(rotate, -y0, x0)
  y1 <- ifelse(rotate, x0, y0)

  out[[x_col]] <- x1 * scale
  out[[y_col]] <- y1 * scale

  out$cx <- NULL
  out$cy <- NULL
  out$rotate <- NULL
  out$scale <- NULL
  out
}

#' Apply panel-wise transforms to segment geometry
#'
#' @param dat Data frame with columns \code{panel}, \code{x}, \code{y},
#'   \code{xend}, \code{yend}.
#' @param transforms Output of [.compute_panel_layout_transforms()].
#' @return Transformed data frame.
#' @keywords internal
#' @noRd
.apply_panel_layout_to_segments <- function(dat, transforms) {
  if (is.null(dat) || nrow(dat) == 0) return(dat)
  if (is.null(transforms) || nrow(transforms) == 0) return(dat)
  if (!all(c("panel", "x", "y", "xend", "yend") %in% names(dat))) return(dat)

  out <- dplyr::left_join(
    dat,
    transforms,
    by = "panel"
  )

  cx <- out$cx
  cy <- out$cy
  rotate <- out$rotate
  scale <- out$scale

  missing_t <- is.na(cx) | is.na(cy) | is.na(scale)
  cx[missing_t] <- 0
  cy[missing_t] <- 0
  rotate[is.na(rotate)] <- FALSE
  scale[missing_t] <- 1

  apply_xy <- function(x, y) {
    x0 <- x - cx
    y0 <- y - cy
    x1 <- ifelse(rotate, -y0, x0)
    y1 <- ifelse(rotate, x0, y0)
    list(x = x1 * scale, y = y1 * scale)
  }

  p1 <- apply_xy(out$x, out$y)
  p2 <- apply_xy(out$xend, out$yend)

  out$x <- p1$x
  out$y <- p1$y
  out$xend <- p2$x
  out$yend <- p2$y

  out$cx <- NULL
  out$cy <- NULL
  out$rotate <- NULL
  out$scale <- NULL
  out
}


.normalize_colorbar_position <- function(colorbar, default = "right") {
  if (is.logical(colorbar) && length(colorbar) == 1L && !is.na(colorbar)) {
    return(if (isTRUE(colorbar)) default else "none")
  }

  if (is.character(colorbar) && length(colorbar) == 1L && !is.na(colorbar)) {
    colorbar <- tolower(colorbar)
    if (colorbar %in% c("none", "right", "bottom")) {
      return(colorbar)
    }
  }

  stop(
    "'colorbar' must be TRUE, FALSE, or one of 'right', 'bottom', 'none'.",
    call. = FALSE
  )
}

.resolve_plot_brain_panel_labels <- function(panel_levels, panel_labels = NULL) {
  if (is.null(panel_labels)) {
    return(panel_levels)
  }

  if (is.function(panel_labels)) {
    return(vapply(panel_levels, panel_labels, character(1), USE.NAMES = FALSE))
  }

  if (!is.character(panel_labels)) {
    stop(
      "'panel_labels' must be NULL, a character vector, or a function.",
      call. = FALSE
    )
  }

  if (is.null(names(panel_labels))) {
    if (length(panel_labels) != length(panel_levels)) {
      stop(
        "Unnamed 'panel_labels' must have length ", length(panel_levels), ".",
        call. = FALSE
      )
    }
    return(panel_labels)
  }

  out <- stats::setNames(panel_levels, panel_levels)
  matched <- intersect(names(panel_labels), panel_levels)
  out[matched] <- unname(panel_labels[matched])
  unname(out)
}

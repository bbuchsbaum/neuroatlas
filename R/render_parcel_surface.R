# CPU parcel-map rendering for plot_brain(static_backend = "cpu", vals = ...).
#
# Each panel is rasterized by neurosurf::render_surface_parcels(), which
# resolves parcels per supersample from smoothed label memberships (smooth,
# antialiased boundaries), shades a sulcal-depth underlay with soft lighting,
# and returns straight-alpha RGBA. Panels are composed by a grid figure whose
# layout is computed at draw time from the device size, so the same object
# prints cleanly into any PNG/PDF canvas.

.parcel_prep_cache <- new.env(parent = emptyenv())

.resolve_parcel_anatomy <- function(surfatlas, hemi, override = NULL) {
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  display <- atlas_hemi@geometry
  n <- length(atlas_hemi@data)
  metric <- .surface_hemi_value(override, hemi) %||%
    .surface_hemi_value(surfatlas$anatomy_metric, hemi)
  if (!is.null(metric)) {
    if (!is.numeric(metric) || length(metric) != n || any(!is.finite(metric))) {
      stop("Anatomy metric for ", hemi,
           " must contain one finite value per vertex.", call. = FALSE)
    }
    return(list(metric = as.numeric(metric), source = "explicit"))
  }
  surf_type <- surfatlas$surf_type %||% "inflated"
  if (!identical(surf_type, "white")) {
    # Same white-surface lookup as the stat renderer (packaged fsaverage6
    # meshes first, TemplateFlow otherwise).
    white <- tryCatch(
      .resolve_overlay_surface_pair(surfatlas, hemi = hemi)$white,
      error = function(e) NULL
    )
    if (!is.null(white) && .surface_geometry_topology_equal(white, display)) {
      return(list(
        metric = neurosurf::surface_sulcal_proxy(white, display),
        source = "sulcal_proxy_white_vs_display"
      ))
    }
  }
  fallback <- .resolve_surface_anatomy(surfatlas, hemi)
  list(metric = fallback$metric, source = fallback$provenance$source)
}

.parcel_surface_prep <- function(surfatlas, hemi, cortex_mask = NULL,
                                 anatomy_metric = NULL, boundary_smooth = 3L) {
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  geometry <- atlas_hemi@geometry
  labels <- as.integer(atlas_hemi@data)
  mask <- .surface_hemi_value(cortex_mask, hemi)
  key <- rlang::hash(list(
    geometry@mesh$vb[1:3, , drop = FALSE], geometry@mesh$it, labels,
    surfatlas$surface_space, surfatlas$surf_type, mask,
    .surface_hemi_value(anatomy_metric, hemi), boundary_smooth
  ))
  cached <- .parcel_prep_cache[[key]]
  if (!is.null(cached)) return(cached)
  anatomy <- .resolve_parcel_anatomy(surfatlas, hemi, override = anatomy_metric)
  prep <- neurosurf::prepare_surface_parcels(
    geometry, labels,
    anatomy_metric = anatomy$metric,
    cortex_mask = if (is.null(mask)) NULL else as.logical(mask),
    boundary_smooth = boundary_smooth
  )
  prep$anatomy_source <- anatomy$source
  assign(key, prep, envir = .parcel_prep_cache)
  prep
}

.plot_brain_cpu_parcels <- function(surfatlas,
                                    vals,
                                    views,
                                    hemis,
                                    palette,
                                    lim,
                                    threshold = NULL,
                                    colorbar_position = "bottom",
                                    colorbar_title = NULL,
                                    title = NULL,
                                    subtitle = NULL,
                                    caption = NULL,
                                    panel_labels = NULL,
                                    bg = "white",
                                    cortex_mask = NULL,
                                    anatomy_metric = NULL,
                                    camera = "canonical",
                                    render_width = 900L,
                                    render_height = 620L,
                                    render_antialias = 3L,
                                    parcel_style = NULL) {
  ids <- as.integer(surfatlas$ids)
  values <- stats::setNames(as.numeric(vals), ids)
  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1L ||
        !is.finite(threshold) || threshold < 0) {
      stop("'vals_threshold' must be a non-negative number.", call. = FALSE)
    }
    values[is.finite(values) & abs(values) < threshold] <- NA_real_
  }
  finite <- values[is.finite(values)]
  if (is.null(lim)) {
    m <- if (length(finite)) max(abs(finite)) else 1
    if (m == 0) m <- 1
    lim <- c(-m, m)
  }
  palette_cols <- if (length(palette) > 1L) palette else
    scico::scico(256, palette = palette)
  style <- if (inherits(parcel_style, "surface_parcel_style")) {
    parcel_style
  } else {
    do.call(neurosurf::surface_parcel_style, as.list(parcel_style %||% list()))
  }

  panels <- list()
  keys <- character()
  provenance <- list(anatomy = list(), camera = list())
  for (view in views) for (hemi in hemis) {
    hk <- if (hemi == "left") "lh" else "rh"
    prep <- .parcel_surface_prep(surfatlas, hk, cortex_mask = cortex_mask,
                                 anatomy_metric = anatomy_metric)
    rendered <- neurosurf::render_surface_parcels(
      prep, values = values, camera = view, camera_mode = camera,
      width = render_width, height = render_height,
      antialias = render_antialias, palette = palette_cols, limits = lim,
      style = style
    )
    key <- paste0(hk, "_", view)
    default_label <- paste0(tools::toTitleCase(hemi), " ",
                            tools::toTitleCase(view))
    panels[[key]] <- list(
      raster = .parcel_rgba_colors(rendered),
      label = default_label,
      hemi = hk, view = view
    )
    keys <- c(keys, key)
    provenance$anatomy[[hk]] <- list(source = prep$anatomy_source)
    provenance$camera[[key]] <- rendered$camera
  }

  resolved <- .resolve_plot_brain_panel_labels(
    vapply(panels, `[[`, "", "label"), panel_labels
  )
  for (k in seq_along(panels)) panels[[k]]$label <- unname(resolved[[k]])

  # A map with nothing to show gets no colorbar: a legend spanning an empty
  # range reads as data.
  if (!length(finite)) colorbar_position <- "none"
  colorbar <- if (identical(colorbar_position, "none")) NULL else list(
    palette = palette_cols, lim = lim, title = colorbar_title,
    threshold = threshold, position = colorbar_position
  )
  fig <- .parcel_figure_grob(
    panels = panels, views = views, hemis = hemis, colorbar = colorbar,
    title = title, subtitle = subtitle, caption = caption, bg = bg,
    per_panel_labels = !is.null(panel_labels)
  )
  structure(fig,
    plot_brain_backend = "cpu_deferred_parcels",
    plot_brain_colorbar = list(source = "base", palette = palette, lim = lim,
                               title = colorbar_title, threshold = threshold),
    plot_brain_anatomy = provenance$anatomy,
    plot_brain_camera = provenance$camera,
    plot_brain_parcel_style = unclass(style)
  )
}

.parcel_rgba_colors <- function(rendered) {
  rgba <- rendered$rgba
  channel <- function(k) as.integer(rgba[, , k])
  cols <- grDevices::rgb(channel(1), channel(2), channel(3), channel(4),
                         maxColorValue = 255)
  matrix(cols, nrow = dim(rgba)[1], ncol = dim(rgba)[2])
}

# Crop a colour-matrix raster to its non-transparent extent.
.crop_raster_alpha <- function(raster, pad = 4L) {
  alpha <- substr(raster, 8L, 9L)
  ink <- matrix(alpha != "00", nrow(raster), ncol(raster))
  rows <- which(rowSums(ink) > 0L)
  cols <- which(colSums(ink) > 0L)
  if (!length(rows)) return(raster)
  r <- max(1L, min(rows) - pad):min(nrow(raster), max(rows) + pad)
  c <- max(1L, min(cols) - pad):min(ncol(raster), max(cols) + pad)
  raster[r, c, drop = FALSE]
}

.parcel_figure_grob <- function(panels, views, hemis, colorbar, title,
                                subtitle, caption, bg,
                                per_panel_labels = FALSE) {
  for (k in names(panels)) {
    panels[[k]]$raster <- .crop_raster_alpha(panels[[k]]$raster)
  }
  grid::gTree(
    panels = panels, views = views, hemis = hemis, colorbar = colorbar,
    title = title, subtitle = subtitle, caption = caption, bg = bg,
    per_panel_labels = per_panel_labels, cl = "parcel_brain_figure"
  )
}

# Candidate arrangements. A views x hemis grid (and its transpose) is labelled
# with column and row headers; a single row labels each panel. For
# lateral+medial pairs the row is ordered L lateral, L medial, R medial,
# R lateral so the medial walls face each other.
.parcel_figure_arrangements <- function(views, hemis) {
  hk <- ifelse(hemis == "left", "lh", "rh")
  hemi_names <- ifelse(hemis == "left", "Left", "Right")
  view_names <- tools::toTitleCase(views)
  grid_keys <- matrix(
    unlist(lapply(views, function(v) paste0(hk, "_", v))),
    nrow = length(views), byrow = TRUE
  )
  out <- list(list(keys = grid_keys, col_names = hemi_names,
                   row_names = view_names))
  if (length(views) > 1L && length(hemis) > 1L) {
    out <- c(out, list(list(keys = t(grid_keys), col_names = view_names,
                            row_names = hemi_names)))
  }
  if (length(views) * length(hemis) > 1L) {
    row <- if (setequal(views, c("lateral", "medial")) &&
               setequal(hemis, c("left", "right"))) {
      c("lh_lateral", "lh_medial", "rh_medial", "rh_lateral")
    } else {
      as.vector(t(grid_keys))
    }
    out <- c(out, list(list(keys = matrix(row, nrow = 1L), col_names = NULL,
                            row_names = NULL)))
  }
  out
}

.parcel_figure_metrics <- function(x, fig_w, fig_h) {
  # Text keeps its point size down to 6 in wide, then shrinks with the canvas.
  em <- min(1, max(0.6, fig_w / 6))
  list(
    em = em,
    side = 0.22 * em,
    label_h = 0.2 * em,
    row_label_w = 0.24 * em,
    gutter_x = 0.2 * em,
    gutter_y = 0.08 * em,
    head_h = if (is.null(x$title)) 0.1 * em else
      if (is.null(x$subtitle)) 0.46 * em else 0.66 * em,
    foot_h = if (is.null(x$caption)) 0 else 0.28 * em,
    cb_bottom_h = 0.58 * em,
    cb_right_w = 0.95 * em
  )
}

# Fit one arrangement + colorbar placement; returns the scale (in/px) and
# the geometry needed to draw it.
.parcel_figure_fit <- function(arr, dims, per_panel_labels, cb_pos, m,
                               fig_w, fig_h) {
  keys <- arr$keys
  headers <- !per_panel_labels && !is.null(arr$col_names) &&
    (nrow(keys) > 1L || ncol(keys) > 1L)
  colw <- apply(keys, 2, function(k) max(vapply(k, function(z) dims[[z]][2], 1)))
  rowh <- apply(keys, 1, function(k) max(vapply(k, function(z) dims[[z]][1], 1)))
  lab_rows <- if (headers) 1L else nrow(keys)
  left_w <- if (headers && nrow(keys) > 1L) m$row_label_w else 0
  avail_w <- fig_w - 2 * m$side - left_w -
    if (identical(cb_pos, "right")) m$cb_right_w else 0
  avail_h <- fig_h - m$head_h - m$foot_h -
    if (identical(cb_pos, "bottom")) m$cb_bottom_h else 0.1 * m$em
  s <- min((avail_w - m$gutter_x * (ncol(keys) - 1)) / sum(colw),
           (avail_h - m$gutter_y * (nrow(keys) - 1) - m$label_h * lab_rows) /
             sum(rowh))
  list(arr = arr, headers = headers, colw = colw, rowh = rowh, s = s,
       left_w = left_w, avail_w = avail_w, avail_h = avail_h, cb_pos = cb_pos)
}

#' @export
#' @importFrom grid makeContent
makeContent.parcel_brain_figure <- function(x) {
  inch <- function(v) grid::unit(v, "in")
  fig_w <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  fig_h <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  ink <- "#1F2328"
  muted <- "#4B5563"
  m <- .parcel_figure_metrics(x, fig_w, fig_h)
  em <- m$em
  cb <- x$colorbar
  if (!is.null(cb)) {
    text_w <- function(txt, size) {
      max(vapply(txt, function(t) grid::convertWidth(
        grid::stringWidth(t), "in", valueOnly = TRUE
      ) * size / grid::get.gpar("fontsize")$fontsize, 1))
    }
    ticks <- .parcel_colorbar_ticks(cb$lim, cb$threshold)
    tick_w <- text_w(ticks$labels, 9 * em)
    title_w <- if (is.null(cb$title)) 0 else text_w(cb$title, 9.5 * em)
    m$cb_right_w <- 0.3 * em + max(0.1 * em + 0.08 * em + tick_w, title_w) +
      0.05 * em
  }
  cb_choices <- if (is.null(cb)) "none" else if (identical(cb$position, "auto")) {
    c("bottom", "right")
  } else {
    cb$position
  }
  dims <- lapply(x$panels, function(p) dim(p$raster))
  best <- NULL
  for (arr in .parcel_figure_arrangements(x$views, x$hemis)) {
    for (pos in cb_choices) {
      fit <- .parcel_figure_fit(arr, dims, isTRUE(x$per_panel_labels), pos, m,
                                fig_w, fig_h)
      # Prefer a bottom colorbar unless the right one buys >4% more scale.
      bonus <- if (identical(pos, "right")) 0.96 else 1
      if (is.null(best) || fit$s * bonus > best$s * best$bonus) {
        best <- fit
        best$bonus <- bonus
      }
    }
  }
  s <- max(best$s, 1e-6)
  keys <- best$arr$keys
  lab_rows <- if (best$headers) 1L else nrow(keys)
  grid_w <- sum(best$colw) * s + m$gutter_x * (ncol(keys) - 1)
  grid_h <- sum(best$rowh) * s + m$gutter_y * (nrow(keys) - 1) +
    m$label_h * lab_rows
  block_w <- best$left_w + grid_w +
    if (identical(best$cb_pos, "right")) m$cb_right_w else 0
  x_block <- m$side + max(0, (fig_w - 2 * m$side - block_w) / 2)
  x0 <- x_block + best$left_w
  top <- fig_h - m$head_h - max(0, (best$avail_h - grid_h) / 2)

  kids <- list(grid::rectGrob(gp = grid::gpar(fill = x$bg, col = NA)))
  if (!is.null(x$title)) {
    kids <- c(kids, list(grid::textGrob(
      x$title, x = inch(x_block), y = inch(fig_h - 0.27 * em),
      just = c("left", "center"),
      gp = grid::gpar(fontsize = 13 * em, fontface = "bold", col = ink)
    )))
    if (!is.null(x$subtitle)) {
      kids <- c(kids, list(grid::textGrob(
        x$subtitle, x = inch(x_block), y = inch(fig_h - 0.48 * em),
        just = c("left", "center"),
        gp = grid::gpar(fontsize = 9.5 * em, col = "#5A6270")
      )))
    }
  }
  label_gp <- grid::gpar(fontsize = 9.5 * em, col = muted)
  if (best$headers) {
    xc <- x0
    for (c in seq_len(ncol(keys))) {
      kids <- c(kids, list(grid::textGrob(
        best$arr$col_names[c], x = inch(xc + best$colw[c] * s / 2),
        y = inch(top - m$label_h + 0.05 * em), just = c("center", "bottom"),
        gp = label_gp
      )))
      xc <- xc + best$colw[c] * s + m$gutter_x
    }
  }
  y <- top - if (best$headers) m$label_h else 0
  for (r in seq_len(nrow(keys))) {
    if (!best$headers) y <- y - m$label_h
    if (best$headers && nrow(keys) > 1L) {
      kids <- c(kids, list(grid::textGrob(
        best$arr$row_names[r], x = inch(x0 - 0.1 * em),
        y = inch(y - best$rowh[r] * s / 2), rot = 90,
        just = c("center", "bottom"), gp = label_gp
      )))
    }
    xc <- x0
    for (c in seq_len(ncol(keys))) {
      p <- x$panels[[keys[r, c]]]
      d <- dim(p$raster)
      cx <- xc + best$colw[c] * s / 2
      brain_top <- y - (best$rowh[r] - d[1]) * s / 2
      if (!best$headers && !is.null(p$label) && nzchar(p$label)) {
        kids <- c(kids, list(grid::textGrob(
          p$label, x = inch(cx), y = inch(brain_top + 0.03 * em),
          just = c("center", "bottom"), gp = label_gp
        )))
      }
      kids <- c(kids, list(grid::rasterGrob(
        p$raster, x = inch(cx), y = inch(brain_top),
        width = inch(d[2] * s), height = inch(d[1] * s),
        just = c("center", "top"), interpolate = TRUE
      )))
      xc <- xc + best$colw[c] * s + m$gutter_x
    }
    y <- y - best$rowh[r] * s - m$gutter_y
  }
  if (!is.null(cb)) {
    cb$position <- best$cb_pos
    kids <- c(kids, .parcel_colorbar_grobs(
      cb, fig_w = fig_w, fig_h = fig_h, em = em,
      grid_left = x0, grid_right = x0 + grid_w,
      grid_mid = top - grid_h / 2, grid_bottom = top - grid_h,
      bottom = m$foot_h, ink = ink, muted = muted
    ))
  }
  if (!is.null(x$caption)) {
    kids <- c(kids, list(grid::textGrob(
      x$caption, x = inch(x_block), y = inch(0.14 * em),
      just = c("left", "center"),
      gp = grid::gpar(fontsize = 8 * em, col = muted)
    )))
  }
  grid::setChildren(x, do.call(grid::gList, kids))
}

.parcel_colorbar_ticks <- function(lim, threshold) {
  # Greedy placement by priority (limits, threshold, zero, round numbers) so
  # labels never crowd: each accepted tick keeps the others at least `clear`
  # away. Limits are labelled at their nearest round value when close.
  in_range <- function(x) x[x >= lim[1] - 1e-9 & x <= lim[2] + 1e-9]
  thr <- if (!is.null(threshold) && threshold > 0) c(-threshold, threshold)
  # Round toward zero so a rounded limit label never falls outside the bar.
  mag <- 10^(floor(log10(pmax(abs(lim), .Machine$double.eps))) - 1)
  ends <- sign(lim) * floor(abs(lim) / mag) * mag
  ends <- ends[ends >= lim[1] - 1e-9 & ends <= lim[2] + 1e-9]
  candidates <- unique(c(in_range(thr), 0, ends, in_range(pretty(lim, 6))))
  candidates <- in_range(candidates)
  clear <- 0.14 * diff(lim)
  ticks <- numeric(0)
  for (v in candidates) {
    if (!length(ticks) || all(abs(ticks - v) >= clear - 1e-9)) {
      ticks <- c(ticks, v)
    }
  }
  if (!is.null(threshold) && threshold > 0) {
    # Inside the grey sub-threshold band only zero is informative.
    ticks <- ticks[ticks == 0 | abs(ticks) >= threshold - 1e-9]
  }
  ticks <- sort(ticks)
  labels <- trimws(formatC(ticks, format = "fg", digits = 3))
  list(at = ticks, labels = sub("^-", "\u2212", labels))
}

.parcel_colorbar_grobs <- function(cb, fig_w, fig_h, em, grid_left,
                                   grid_right, grid_mid, grid_bottom, bottom,
                                   ink, muted) {
  inch <- function(v) grid::unit(v, "in")
  ramp <- grDevices::colorRampPalette(cb$palette, space = "Lab")(512)
  vv <- seq(cb$lim[1], cb$lim[2], length.out = 512)
  if (!is.null(cb$threshold) && cb$threshold > 0) {
    ramp[abs(vv) < cb$threshold] <- "#E3E3E0"
  }
  ticks <- .parcel_colorbar_ticks(cb$lim, cb$threshold)
  pos <- (ticks$at - cb$lim[1]) / diff(cb$lim)
  tick_gp <- grid::gpar(col = muted, lwd = 0.5)
  # Threshold ticks mark the inferential boundary, so they are set bold.
  thr <- cb$threshold
  is_thr <- if (!is.null(thr) && thr > 0) {
    abs(abs(ticks$at) - thr) < 1e-9
  } else {
    rep(FALSE, length(ticks$at))
  }
  text_gp <- grid::gpar(fontsize = 9 * em,
                        col = ifelse(is_thr, ink, muted),
                        fontface = ifelse(is_thr, "bold", "plain"))
  title_gp <- grid::gpar(fontsize = 9.5 * em, col = ink)
  if (identical(cb$position, "right")) {
    bh <- min(2.2 * em, fig_h * 0.5)
    bw <- 0.1 * em
    cx <- grid_right + 0.3 * em
    cy <- grid_mid
    ty <- cy - bh / 2 + bh * pos
    out <- list(
      grid::rasterGrob(matrix(rev(ramp), ncol = 1), x = inch(cx), y = inch(cy),
                       width = inch(bw), height = inch(bh),
                       interpolate = FALSE),
      grid::rectGrob(x = inch(cx), y = inch(cy), width = inch(bw),
                     height = inch(bh),
                     gp = grid::gpar(col = "#9CA3AF", fill = NA, lwd = 0.4)),
      grid::segmentsGrob(inch(cx + bw / 2), inch(ty),
                         inch(cx + bw / 2 + 0.035 * em), inch(ty), gp = tick_gp),
      grid::textGrob(ticks$labels, x = inch(cx + bw / 2 + 0.06 * em),
                     y = inch(ty), just = c("left", "center"), gp = text_gp)
    )
    if (!is.null(cb$title)) {
      out <- c(out, list(grid::textGrob(
        cb$title, x = inch(cx - bw / 2), y = inch(cy + bh / 2 + 0.08 * em),
        just = c("left", "bottom"), gp = title_gp
      )))
    }
    return(out)
  }
  bw <- min(3.6 * em, max(1.4 * em, 0.45 * (grid_right - grid_left)))
  bh <- 0.1 * em
  cx <- (grid_left + grid_right) / 2
  cy <- max(bottom + 0.28 * em, grid_bottom - 0.3 * em)
  tx <- cx - bw / 2 + bw * pos
  out <- list(
    grid::rasterGrob(matrix(ramp, nrow = 1), x = inch(cx), y = inch(cy),
                     width = inch(bw), height = inch(bh), interpolate = FALSE),
    grid::rectGrob(x = inch(cx), y = inch(cy), width = inch(bw),
                   height = inch(bh),
                   gp = grid::gpar(col = "#9CA3AF", fill = NA, lwd = 0.4)),
    grid::segmentsGrob(inch(tx), inch(cy - bh / 2), inch(tx),
                       inch(cy - bh / 2 - 0.035 * em), gp = tick_gp),
    grid::textGrob(ticks$labels, x = inch(tx),
                   y = inch(cy - bh / 2 - 0.06 * em),
                   just = c("center", "top"), gp = text_gp)
  )
  if (!is.null(cb$title)) {
    out <- c(out, list(grid::textGrob(
      cb$title, x = inch(cx), y = inch(cy + bh / 2 + 0.05 * em),
      just = c("center", "bottom"), gp = title_gp
    )))
  }
  out
}

#' @export
print.parcel_brain_figure <- function(x, newpage = TRUE, ...) {
  if (isTRUE(newpage)) grid::grid.newpage()
  grid::grid.draw(x)
  invisible(x)
}

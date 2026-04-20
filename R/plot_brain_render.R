# Final composition layer for plot_brain().
#
# These two helpers turn the polygon/overlay/boundary geometry into a
# rendered figure: .compose_plot_brain_figure stitches the main brain
# panel together with optional colour-bar and annotation theming via
# patchwork (when available) or plain ggplot composition; and
# .make_colorbar_panel produces the standalone colour-bar ggplot grob
# attached to the main panel.
#
# Split out from plot_brain.R; behaviour is intentionally unchanged.

.compose_plot_brain_figure <- function(main_plot,
                                       colorbar_plot = NULL,
                                       colorbar_position = "none",
                                       title = NULL,
                                       subtitle = NULL,
                                       caption = NULL,
                                       bg = "white") {
  is_patchwork_plot <- inherits(main_plot, "patchwork")
  annotation_theme <- ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = bg, colour = NA),
    plot.title = ggplot2::element_text(face = "bold",
                                       margin = ggplot2::margin(b = 6)),
    plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 8)),
    plot.caption = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

  if (identical(colorbar_position, "none") || is.null(colorbar_plot)) {
    if (is_patchwork_plot) {
      return(main_plot + patchwork::plot_annotation(
        title = title,
        subtitle = subtitle,
        caption = caption,
        theme = annotation_theme
      ))
    }

    return(
      main_plot +
        ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
        annotation_theme
    )
  }

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required when 'colorbar' is not 'none'. ",
      "Install it with install.packages('patchwork').",
      call. = FALSE
    )
  }

  combined <- if (identical(colorbar_position, "bottom")) {
    patchwork::wrap_plots(main_plot, colorbar_plot, ncol = 1,
                          heights = c(1, 0.16))
  } else {
    patchwork::wrap_plots(main_plot, colorbar_plot, ncol = 2,
                          widths = c(1, 0.12))
  }

  combined + patchwork::plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = annotation_theme
  )
}

#' Build a standalone colorbar ggplot panel
#'
#' @param palette scico palette name.
#' @param lim Numeric length-2 colour limits.
#' @param title Optional title string for the colour guide.
#' @param position Colorbar placement keyword. One of \code{"right"} or
#'   \code{"bottom"}.
#' @param bg Background colour.
#' @return A \code{ggplot2} object containing only a colour bar.
#' @keywords internal
#' @noRd
.make_colorbar_panel <- function(palette,
                                 lim,
                                 title = NULL,
                                 position = c("right", "bottom"),
                                 bg = "white") {
  position <- match.arg(position)
  .require_suggest("scico", feature = "colorbar rendering")
  guide <- if (identical(position, "bottom")) {
    ggplot2::guide_colorbar(
      barwidth = ggplot2::unit(10, "lines"),
      barheight = ggplot2::unit(0.9, "lines"),
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal"
    )
  } else {
    ggplot2::guide_colorbar(
      barwidth = ggplot2::unit(0.9, "lines"),
      barheight = ggplot2::unit(8, "lines"),
      title.position = "top",
      title.hjust = 0.5,
      direction = "vertical"
    )
  }
  dummy <- data.frame(x = 1, y = 1, fill = mean(lim))
  p <- ggplot2::ggplot(dummy, ggplot2::aes(x = x, y = y, fill = fill)) +
    ggplot2::geom_point(alpha = 0, show.legend = TRUE) +
    scico::scale_fill_scico(
      palette = palette, limits = lim, oob = scales::squish,
      name = title,
      guide = guide
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = position,
      plot.background = ggplot2::element_rect(fill = NA, colour = NA)
    )
  cowplot_extract <- function(pl) {
    gt <- ggplot2::ggplotGrob(pl)
    guide_idx <- grep("guide", gt$layout$name)
    if (length(guide_idx) == 0) return(pl)
    guide_grob <- gt$grobs[[guide_idx[1]]]
    ggplot2::ggplot() + ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = NA, colour = NA)) +
      ggplot2::annotation_custom(guide_grob)
  }
  cowplot_extract(p)
}

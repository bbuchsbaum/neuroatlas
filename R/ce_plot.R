.cluster_explorer_demo_inputs <- function(n_time = 48L) {
  dims <- c(12L, 12L, 12L)
  sp3 <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[2:4, 2:4, 2:4] <- 1L
  atlas_arr[8:10, 2:4, 2:4] <- 2L
  atlas_arr[2:4, 8:10, 2:4] <- 3L
  atlas_arr[8:10, 8:10, 2:4] <- 4L
  atlas_arr[2:4, 5:7, 8:10] <- 5L
  atlas_arr[8:10, 5:7, 8:10] <- 6L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas <- list(
    name = "demo_atlas",
    atlas = atlas_vol,
    ids = 1:6,
    labels = c("Frontal-A", "Frontal-B", "Parietal-A",
               "Parietal-B", "Temporal-A", "Temporal-B"),
    orig_labels = c("Frontal-A", "Frontal-B", "Parietal-A",
                    "Parietal-B", "Temporal-A", "Temporal-B"),
    hemi = c("left", "right", "left", "right", "left", "right"),
    roi_metadata = tibble::tibble(
      id = 1:6,
      label = c("Frontal-A", "Frontal-B", "Parietal-A",
                "Parietal-B", "Temporal-A", "Temporal-B"),
      hemi = c("left", "right", "left", "right", "left", "right")
    )
  )
  class(atlas) <- c("demo", "atlas")

  stat_arr <- array(0, dim = dims)
  stat_arr[2:4, 2:4, 2:4] <- 4.8
  stat_arr[8:10, 5:7, 8:10] <- -5.1
  stat_arr[6, 6, 6] <- 8.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp3)

  sp4 <- neuroim2::NeuroSpace(
    dim = c(dims, n_time),
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  data_arr <- array(0, dim = c(dims, n_time))
  p_mask <- atlas_arr == 1L
  n_mask <- atlas_arr == 6L
  bg_mask <- atlas_arr %in% c(2L, 3L, 4L, 5L)

  t_idx <- seq_len(n_time)
  p_sig <- 0.5 + 0.7 * sin(t_idx / 5)
  n_sig <- 1.2 + 0.55 * cos(t_idx / 7)
  bg_sig <- 0.2 + 0.15 * sin(t_idx / 9)

  for (t in t_idx) {
    vol_t <- data_arr[,,, t]
    vol_t[p_mask] <- p_sig[t]
    vol_t[n_mask] <- n_sig[t]
    vol_t[bg_mask] <- bg_sig[t]
    data_arr[,,, t] <- vol_t
  }

  data_source <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    sample_id = sprintf("sample_%03d", t_idx),
    time = t_idx,
    run = factor(ifelse(t_idx <= n_time / 2, "run1", "run2")),
    condition = factor(rep(c("A", "B", "C"), length.out = n_time))
  )

  design <- tibble::tibble(
    task_load = as.numeric(scale(t_idx)),
    cue = factor(rep(c("cue", "probe"), length.out = n_time))
  )

  surfatlas <- list(
    name = "demo_surfatlas",
    ids = atlas$ids,
    labels = atlas$labels,
    hemi = atlas$hemi
  )
  class(surfatlas) <- c("demo_surfatlas", "surfatlas", "atlas")

  list(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    surfatlas = surfatlas,
    sample_table = sample_table,
    design = design
  )
}

.fallback_brain_plot <- function(surfatlas,
                                 vals = NULL,
                                 palette = "vik",
                                 lim = NULL,
                                 interactive = TRUE,
                                 title = "Parcel Layout (Fallback)") {
  ids <- as.integer(surfatlas$ids)
  if (length(ids) == 0) {
    ids <- seq_len(if (!is.null(vals)) length(vals) else 12L)
  }

  labels <- as.character(surfatlas$labels)
  if (length(labels) != length(ids)) {
    labels <- paste0("Parcel ", ids)
  }

  if (is.null(vals)) {
    value <- rep(0, length(ids))
  } else {
    v <- as.numeric(vals)
    if (length(v) != length(ids)) {
      value <- rep(NA_real_, length(ids))
      names(value) <- ids
      common <- intersect(as.character(ids), names(vals))
      value[common] <- as.numeric(vals[common])
    } else {
      value <- v
    }
  }

  n <- length(ids)
  ncol <- max(1L, ceiling(sqrt(n)))
  nrow <- ceiling(n / ncol)
  grid_idx <- seq_len(n)
  cx <- ((grid_idx - 1L) %% ncol) + 1L
  cy <- nrow - ((grid_idx - 1L) %/% ncol)

  dat <- tibble::tibble(
    parcel_id = ids,
    label = labels,
    value = value,
    x = cx,
    y = cy,
    data_id = as.character(ids),
    tooltip = paste0(labels, "\nValue: ", signif(value, 4))
  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y))
  if (isTRUE(interactive)) {
    p <- p + ggiraph::geom_tile_interactive(
      ggplot2::aes(fill = value, tooltip = tooltip, data_id = data_id),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  } else {
    p <- p + ggplot2::geom_tile(
      ggplot2::aes(fill = value),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  }

  if (n <= 30) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = parcel_id),
      size = 2.8,
      colour = "#111827"
    )
  }

  p <- p +
    scico::scale_fill_scico(
      palette = palette,
      limits = lim,
      oob = scales::squish,
      na.value = "#f3f4f6"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = "Value") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "#6b7280", size = 11),
      legend.position = "none"
    )

  if (!isTRUE(interactive)) {
    return(p)
  }

  ggiraph::girafe(ggobj = p)
}

.build_design_plot <- function(data, x_var, collapse_vars = NULL) {
  collapse_vars <- collapse_vars[collapse_vars %in% names(data)]
  collapse_vars <- setdiff(collapse_vars, c("cluster_id", "signal"))

  plot_data <- data
  if (length(collapse_vars) > 0) {
    group_vars <- unique(c("cluster_id", x_var, collapse_vars))
    plot_data <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(signal = mean(signal, na.rm = TRUE), .groups = "drop")
  }

  x <- plot_data[[x_var]]
  xtype <- infer_design_var_type(x)

  if (xtype == "continuous") {
    if (identical(x_var, ".sample_index") ||
        inherits(x, "Date") || inherits(x, "POSIXt")) {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = cluster_id)
      ) +
        ggplot2::geom_line(alpha = 0.7) +
        ggplot2::geom_point(size = 1.5, alpha = 0.8)
    } else {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = cluster_id)
      ) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::geom_smooth(se = FALSE, method = "loess")
    }
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = factor(.data[[x_var]]),
                   y = signal,
                   color = cluster_id)
    ) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.25) +
      ggplot2::geom_jitter(width = 0.15, alpha = 0.75, size = 1.5)
  }

  if (length(unique(plot_data$cluster_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~ cluster_id, scales = "free_y")
  }

  p +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#f3f4f6", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.4),
      axis.title = ggplot2::element_text(color = "#374151"),
      axis.text = ggplot2::element_text(color = "#111827"),
      strip.background = ggplot2::element_rect(fill = "#f9fafb", color = "#e5e7eb"),
      strip.text = ggplot2::element_text(color = "#111827", face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = "#374151"),
      legend.text = ggplot2::element_text(color = "#111827")
    ) +
    ggplot2::labs(
      x = x_var,
      y = "Cluster Signal",
      color = "Cluster"
    )
}

.empty_plot <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

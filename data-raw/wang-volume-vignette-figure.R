# Regenerate vignettes/figures/wang-volume-native.png.
#
# The maximum-probability maps are categorical labels on the native
# ProbAtlas_v4 grid. Zero is background and must not consume a colour. This
# script deliberately avoids a continuous legend, which would imply that
# numeric label order is scientifically meaningful.

library(neuroatlas)
library(ggplot2)

wang_volumes <- get_wang_prob_atlas(
  image = "maxprob",
  hemi = "both",
  path_only = FALSE
)

left <- as.array(wang_volumes$volumes$lh)
right <- as.array(wang_volumes$volumes$rh)

stopifnot(
  identical(dim(left), dim(right)),
  identical(
    neuroim2::space(wang_volumes$volumes$lh),
    neuroim2::space(wang_volumes$volumes$rh)
  ),
  !any(left > 0 & right > 0),
  all(unique(left[left > 0]) %in% wang_volumes$labels$id),
  all(unique(right[right > 0]) %in% wang_volumes$labels$id)
)

right[right > 0] <- right[right > 0] + 25L
label_array <- left + right

occupied <- which(label_array > 0, arr.ind = TRUE)
slice_ids <- unique(as.integer(stats::quantile(
  occupied[, 3],
  probs = seq(0.08, 0.92, length.out = 6),
  type = 1
)))

x_limits <- range(occupied[, 1]) + c(-7L, 7L)
y_limits <- range(occupied[, 2]) + c(-7L, 7L)
x_limits <- pmax(1L, pmin(dim(label_array)[1], x_limits))
y_limits <- pmax(1L, pmin(dim(label_array)[2], y_limits))

slice_data <- do.call(rbind, lapply(slice_ids, function(z) {
  index <- which(label_array[, , z] > 0, arr.ind = TRUE)
  data.frame(
    x = index[, 1],
    y = index[, 2],
    region_id = label_array[cbind(index[, 1], index[, 2], z)],
    slice = factor(z, levels = slice_ids)
  )
}))

region_names <- c(
  stats::setNames(wang_volumes$labels$label, wang_volumes$labels$id),
  stats::setNames(
    paste0(wang_volumes$labels$label, " (R)"),
    wang_volumes$labels$id + 25L
  )
)
left_names <- stats::setNames(
  paste0(wang_volumes$labels$label, " (L)"),
  wang_volumes$labels$id
)
region_names[names(left_names)] <- left_names

present_ids <- sort(unique(slice_data$region_id))
region_palette <- stats::setNames(
  grDevices::hcl.colors(length(present_ids), "Dynamic", alpha = 1),
  present_ids
)

figure <- ggplot(slice_data, aes(x, y, fill = factor(region_id))) +
  geom_tile(width = 1, height = 1) +
  facet_wrap(~slice, ncol = 3, labeller = label_both) +
  coord_fixed(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  scale_fill_manual(
    values = region_palette,
    labels = region_names[as.character(present_ids)],
    name = NULL,
    drop = FALSE
  ) +
  labs(
    title = "Wang 2015 maximum-probability labels",
    subtitle = "Categorical areas on the native ProbAtlas_v4 voxel grid",
    caption = "L and R identify hemisphere; numeric label order is not quantitative."
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 6.5),
    legend.key.size = grid::unit(0.28, "lines"),
    legend.key.width = grid::unit(0.75, "lines"),
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey30"),
    plot.caption = element_text(hjust = 0.5, colour = "grey35"),
    strip.text = element_text(face = "bold", size = 10),
    panel.background = element_rect(fill = "white", colour = "grey85"),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

ggsave(
  file.path("vignettes", "figures", "wang-volume-native.png"),
  figure,
  width = 10,
  height = 7,
  dpi = 160,
  bg = "white"
)

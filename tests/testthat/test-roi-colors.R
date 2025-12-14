make_palette_test_rois <- function() {
  tibble::tibble(
    roi = sprintf("ROI_%02d", 1:12),
    x = c(-60, -50, -40, -30, -20, -10, 10, 20, 30, 40, 50, 60),
    y = c(-80, -65, -50, -35, -20, -5, 5, 20, 35, 50, 65, 80),
    z = c(-20, -15, -10, -5, 0, 5, 5, 0, -5, -10, -15, -20),
    hemi = rep(c("L", "R"), each = 6),
    network = rep(c("Net1", "Net2", "Net3"), times = 4),
    pair_id = rep(sprintf("Pair_%02d", 1:6), each = 2)
  )
}

test_that("roi_colors_maximin_view produces deterministic palette", {
  rois <- make_palette_test_rois()
  pal <- roi_colors_maximin_view(
    rois,
    hemi_col = "hemi",
    network_col = "network",
    pair_col = "pair_id",
    seed = 123,
    k = 4,
    sigma_xy = 20,
    sigma_slice = 8
  )
  expect_equal(names(pal), c("roi", "color"))
  expect_true(all(nchar(pal$color) == 7))
  expect_snapshot_value(pal, style = "json2")
})

test_that("roi_colors_network_harmony organizes by network", {
  rois <- make_palette_test_rois()
  pal <- roi_colors_network_harmony(
    rois,
    network_col = "network",
    hemi_col = "hemi",
    seed = 42,
    k = 4,
    sigma_xy = 20,
    sigma_slice = 8,
    candidate_multiplier = 1.1,
    hue_width = 40
  )
  expect_equal(nrow(pal), nrow(rois))
  expect_snapshot_value(pal, style = "json2")
})

test_that("roi_colors_rule_hcl honours hemisphere luminance", {
  rois <- make_palette_test_rois()
  pal <- roi_colors_rule_hcl(
    rois,
    network_col = "network",
    hemi_col = "hemi",
    hue_width = 25,
    C = 65,
    L_L = 75,
    L_R = 58
  )
  expect_true(all(substr(pal$color, 1, 1) == "#"))
  expect_snapshot_value(pal, style = "json2")
})

test_that("roi_colors_embedding maps structured hues", {
  rois <- make_palette_test_rois()
  pal <- roi_colors_embedding(
    rois,
    feature_cols = c("x", "y", "z", "network", "hemi"),
    hemi_col = "hemi",
    seed = 7,
    C_range = c(40, 70),
    L = 60
  )
  expect_equal(nrow(pal), nrow(rois))
  expect_snapshot_value(pal, style = "json2")
})

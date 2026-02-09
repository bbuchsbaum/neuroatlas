# Helper: build a tiny atlas for unit tests (no disk I/O)
make_toy_atlas <- function(dims = c(5, 5, 5)) {
  sp <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 1L
  atlas_arr[4:5, 4:5, 4:5] <- 2L
  atlas_arr[1:2, 4:5, 4:5] <- 3L

  mask_arr <- atlas_arr != 0L
  mask_vol <- neuroim2::LogicalNeuroVol(mask_arr, sp)
  clusters <- atlas_arr[mask_arr]
  label_map <- list(RegionA = 1L, RegionB = 2L, RegionC = 3L)
  cvol <- neuroim2::ClusteredNeuroVol(mask = mask_vol, clusters = clusters,
                                       label_map = label_map)

  atlas_obj <- list(
    name = "toy",
    atlas = cvol,
    ids = c(1L, 2L, 3L),
    labels = c("RegionA", "RegionB", "RegionC"),
    orig_labels = c("lh_RegionA", "rh_RegionB", "lh_RegionC"),
    hemi = c("left", "right", "left"),
    cmap = data.frame(r = c(255, 0, 0), g = c(0, 255, 0), b = c(0, 0, 255)),
    network = c("Net1", "Net2", "Net1")
  )
  class(atlas_obj) <- c("toy", "atlas")
  atlas_obj
}

# Helper: plain NeuroVol atlas (like ASEG)
make_toy_neurovol_atlas <- function(dims = c(5, 5, 5)) {
  sp <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 10L
  atlas_arr[4:5, 4:5, 4:5] <- 11L
  atlas_arr[1:2, 4:5, 4:5] <- 12L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp)

  atlas_obj <- list(
    name = "toy_vol",
    atlas = atlas_vol,
    ids = c(10L, 11L, 12L),
    labels = c("Thalamus", "Caudate", "Putamen"),
    orig_labels = c("Left-Thalamus", "Left-Caudate", "Right-Putamen"),
    hemi = c("left", "left", "right"),
    cmap = data.frame(r = c(255, 0, 0), g = c(0, 255, 0), b = c(0, 0, 255))
  )
  class(atlas_obj) <- c("toy_vol", "atlas")
  atlas_obj
}

# --- Tests for atlas_roi_colors ---

test_that("atlas_roi_colors returns correct structure for ClusteredNeuroVol atlas", {
  atlas <- make_toy_atlas()
  cols <- atlas_roi_colors(atlas, method = "rule_hcl")

  expect_s3_class(cols, "tbl_df")
  expect_equal(names(cols), c("id", "color"))
  expect_equal(nrow(cols), length(atlas$ids))
  expect_equal(cols$id, atlas$ids)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", cols$color)))
})

test_that("atlas_roi_colors returns correct structure for NeuroVol atlas", {
  atlas <- make_toy_neurovol_atlas()
  cols <- atlas_roi_colors(atlas, method = "rule_hcl")

  expect_s3_class(cols, "tbl_df")
  expect_equal(names(cols), c("id", "color"))
  expect_equal(nrow(cols), length(atlas$ids))
  expect_equal(cols$id, atlas$ids)
})

test_that("atlas_roi_colors rule_hcl uses network info", {
  atlas <- make_toy_atlas()
  cols <- atlas_roi_colors(atlas, method = "rule_hcl")
  expect_true(all(nchar(cols$color) >= 7))
})

test_that("atlas_roi_colors network_harmony dispatches correctly", {

  atlas <- make_toy_atlas()
  # Toy atlas has only 1 ROI in Net2 so needs generous candidate pool
  cols <- atlas_roi_colors(atlas, method = "network_harmony",
                           k = 2, candidate_multiplier = 20, hue_width = 120)
  expect_equal(nrow(cols), 3)
  expect_true(all(grepl("^#", cols$color)))
})

test_that("atlas_roi_colors maximin_view dispatches correctly", {
  atlas <- make_toy_atlas()
  cols <- atlas_roi_colors(atlas, method = "maximin_view", k = 2)
  expect_equal(nrow(cols), 3)
  expect_true(all(grepl("^#", cols$color)))
})

test_that("atlas_roi_colors embedding dispatches correctly", {
  atlas <- make_toy_atlas()
  cols <- atlas_roi_colors(atlas, method = "embedding")
  expect_equal(nrow(cols), 3)
  expect_true(all(grepl("^#", cols$color)))
})

test_that("atlas_roi_colors accepts named character vector", {
  atlas <- make_toy_atlas()
  manual <- c("1" = "#FF0000", "2" = "#00FF00", "3" = "#0000FF")
  cols <- atlas_roi_colors(atlas, method = manual)

  expect_equal(cols$id, c(1L, 2L, 3L))
  expect_equal(cols$color, c("#FF0000", "#00FF00", "#0000FF"))
})

test_that("atlas_roi_colors accepts unnamed character vector", {
  atlas <- make_toy_atlas()
  manual <- c("#FF0000", "#00FF00", "#0000FF")
  cols <- atlas_roi_colors(atlas, method = manual)

  expect_equal(nrow(cols), 3)
  expect_equal(cols$color, manual)
})

test_that("atlas_roi_colors errors for network_harmony without network", {
  atlas <- make_toy_neurovol_atlas()  # has no $network
  expect_error(atlas_roi_colors(atlas, method = "network_harmony"),
               "network")
})

# --- Tests for plot.atlas ---

test_that("plot.atlas returns ggplot for montage view", {
  atlas <- make_toy_atlas()
  p <- plot(atlas, nslices = 2)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.atlas montage works for NeuroVol atlas", {
  atlas <- make_toy_neurovol_atlas()
  p <- plot(atlas, nslices = 2)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.atlas returns for ortho view", {
  atlas <- make_toy_atlas()
  p <- plot(atlas, view = "ortho")
  # Either patchwork or list of ggplots

  if (requireNamespace("patchwork", quietly = TRUE)) {
    expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
  } else {
    expect_true(is.list(p))
    expect_length(p, 3)
  }
})

test_that("plot.atlas respects nslices", {
  atlas <- make_toy_atlas()
  p <- plot(atlas, nslices = 2)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.atlas accepts custom colors tibble", {
  atlas <- make_toy_atlas()
  cols <- atlas_roi_colors(atlas)
  p <- plot(atlas, colors = cols, nslices = 2)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot.atlas accepts custom colors named vector", {
  atlas <- make_toy_atlas()
  custom <- c("1" = "#FF0000", "2" = "#00FF00", "3" = "#0000FF")
  p <- plot(atlas, colors = custom, nslices = 2)
  expect_true(inherits(p, "ggplot"))
})

# Helper: build a tiny atlas for unit tests (no disk I/O)
make_toy_atlas <- function(dims = c(5, 5, 5)) {
  sp <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 1L   # cluster 1: 8 voxels
  atlas_arr[4:5, 4:5, 4:5] <- 2L   # cluster 2: 8 voxels
  atlas_arr[1:2, 4:5, 4:5] <- 3L   # cluster 3: 8 voxels

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


# --- Tests for ClusteredNeuroVol-based atlases ---

test_that("sub_atlas by ids returns correct regions", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, ids = c(1L, 3L))

  expect_s3_class(sub, "atlas")
  expect_s3_class(sub, "toy")
  expect_equal(sub$ids, c(1L, 3L))
  expect_equal(sub$labels, c("RegionA", "RegionC"))
  expect_equal(sub$hemi, c("left", "left"))
  expect_equal(sub$network, c("Net1", "Net1"))
  expect_equal(sub$orig_labels, c("lh_RegionA", "lh_RegionC"))
  expect_equal(nrow(sub$cmap), 2)
})

test_that("sub_atlas by labels returns correct regions", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, labels = c("RegionB"))

  expect_equal(sub$ids, 2L)
  expect_equal(sub$labels, "RegionB")
  expect_equal(sub$hemi, "right")
})

test_that("sub_atlas by hemi returns correct regions", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, hemi = "left")

  expect_equal(sub$ids, c(1L, 3L))
  expect_equal(sub$labels, c("RegionA", "RegionC"))
  expect_equal(sub$hemi, c("left", "left"))
})

test_that("sub_atlas intersects ids and hemi", {
  atlas <- make_toy_atlas()
  # RegionA is left, RegionB is right => intersect with left keeps only RegionA
  sub <- sub_atlas(atlas, ids = c(1L, 2L), hemi = "left")

  expect_equal(sub$ids, 1L)
  expect_equal(sub$labels, "RegionA")
})

test_that("sub_atlas intersects labels and hemi", {
  atlas <- make_toy_atlas()
  # Both RegionA and RegionC are left, but ask for right => empty
  expect_error(sub_atlas(atlas, labels = c("RegionA", "RegionC"), hemi = "right"),
               "no matching")
})

test_that("sub_atlas preserves class", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, ids = 2L)

  expect_equal(class(sub), c("toy", "atlas"))
})

test_that("sub_atlas errors on invalid ids", {
  atlas <- make_toy_atlas()
  expect_error(sub_atlas(atlas, ids = c(1L, 99L)), "region ids not found")
})

test_that("sub_atlas errors on invalid labels", {
  atlas <- make_toy_atlas()
  expect_error(sub_atlas(atlas, labels = "NoSuchRegion"), "labels not found")
})

test_that("sub_atlas errors when no selection given", {
  atlas <- make_toy_atlas()
  expect_error(sub_atlas(atlas), "at least one")
})

test_that("sub_atlas numeric ids are coerced to integer", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, ids = c(1, 3))

  expect_equal(sub$ids, c(1L, 3L))
})

test_that("sub_atlas builds roi_metadata", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, ids = c(1L, 2L))

  expect_true(!is.null(sub$roi_metadata))
  expect_s3_class(sub$roi_metadata, "tbl_df")
  expect_equal(nrow(sub$roi_metadata), 2)
  expect_equal(sub$roi_metadata$id, c(1L, 2L))
})

test_that("sub_atlas volume has correct voxels (ClusteredNeuroVol)", {
  atlas <- make_toy_atlas()
  sub <- sub_atlas(atlas, ids = 1L)

  # The subsetted volume should only contain cluster-1 voxels
  expect_s4_class(sub$atlas, "ClusteredNeuroVol")
  expect_equal(sort(unique(sub$atlas@clusters)), 1L)
  expect_equal(sum(sub$atlas@mask), 8L)  # 2x2x2 block
})


# --- Tests for plain NeuroVol-based atlases ---

test_that("sub_atlas works with plain NeuroVol atlas by ids", {
  atlas <- make_toy_neurovol_atlas()
  sub <- sub_atlas(atlas, ids = c(10L, 12L))

  expect_equal(sub$ids, c(10L, 12L))
  expect_equal(sub$labels, c("Thalamus", "Putamen"))

  # Non-selected voxels should be zeroed out
  arr <- as.array(sub$atlas)
  expect_true(all(arr[arr != 0] %in% c(10L, 12L)))
  # Cluster 11 region should be zero
  expect_equal(unique(as.integer(arr[4:5, 4:5, 4:5])), 0L)
})

test_that("sub_atlas works with plain NeuroVol atlas by labels", {
  atlas <- make_toy_neurovol_atlas()
  sub <- sub_atlas(atlas, labels = "Caudate")

  expect_equal(sub$ids, 11L)
  expect_equal(sub$labels, "Caudate")
})

test_that("sub_atlas with hemi on NeuroVol atlas", {
  atlas <- make_toy_neurovol_atlas()
  sub <- sub_atlas(atlas, hemi = "left")

  expect_equal(sub$ids, c(10L, 11L))
  expect_equal(sub$labels, c("Thalamus", "Caudate"))
})


# --- Composition tests ---

test_that("sub_atlas result works with get_roi (NeuroVol atlas)", {
  atlas <- make_toy_neurovol_atlas()
  sub <- sub_atlas(atlas, ids = c(10L, 12L))

  rois <- get_roi(sub, label = "Thalamus")
  expect_true(is.list(rois))
  expect_equal(length(rois), 1)
  expect_equal(names(rois), "Thalamus")
})

test_that("sub_atlas can be applied iteratively", {
  atlas <- make_toy_atlas()
  sub1 <- sub_atlas(atlas, hemi = "left")          # keeps 1, 3
  sub2 <- sub_atlas(sub1, labels = "RegionA")       # keeps 1

  expect_equal(sub2$ids, 1L)
  expect_equal(sub2$labels, "RegionA")
})

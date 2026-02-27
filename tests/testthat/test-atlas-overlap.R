# Helper to create a minimal atlas from an integer array
make_overlap_atlas <- function(arr, ids, labels, hemis) {
  sp <- neuroim2::NeuroSpace(dim = dim(arr), spacing = c(1, 1, 1), origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)
  structure(list(
    name = "test",
    atlas = vol,
    ids = ids,
    labels = labels,
    orig_labels = labels,
    hemi = hemis
  ), class = "atlas")
}

test_that("atlas_overlap: identical atlases give Dice=1 and Jaccard=1", {
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:10] <- 1L
  arr[11:25] <- 2L

  atlas_a <- make_overlap_atlas(arr,
    ids = c(1L, 2L),
    labels = c("R1", "R2"),
    hemis = c("left", "right")
  )

  res <- atlas_overlap(atlas_a, atlas_a)

  expect_s3_class(res, "tbl_df")
  # Should have exactly 2 self-overlap rows (region 1 vs 1, region 2 vs 2)
  expect_equal(nrow(res), 2L)
  expect_true(all(res$dice == 1.0))
  expect_true(all(res$jaccard == 1.0))
  # Each region overlaps with itself
  expect_true(all(res$n_overlap == res$n_atlas1))
  expect_true(all(res$n_overlap == res$n_atlas2))
})

test_that("atlas_overlap: non-overlapping atlases return empty result", {
  arr1 <- array(0L, dim = c(5, 5, 5))
  arr1[1:10] <- 1L

  arr2 <- array(0L, dim = c(5, 5, 5))
  arr2[11:25] <- 2L

  atlas_a <- make_overlap_atlas(arr1,
    ids = 1L, labels = "A", hemis = "left"
  )
  atlas_b <- make_overlap_atlas(arr2,
    ids = 2L, labels = "B", hemis = "right"
  )

  res <- atlas_overlap(atlas_a, atlas_b)

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
})

test_that("atlas_overlap: known partial overlap gives correct metrics", {
  # Region 1 in atlas_a: voxels 1-20 (20 voxels)
  # Region 1 in atlas_b: voxels 11-30 (20 voxels)
  # Overlap: voxels 11-20 (10 voxels)
  # Dice = 2*10/(20+20) = 0.5
  # Jaccard = 10/(20+20-10) = 10/30 = 1/3
  arr1 <- array(0L, dim = c(10, 5, 5))
  arr1[1:20] <- 1L

  arr2 <- array(0L, dim = c(10, 5, 5))
  arr2[11:30] <- 1L

  atlas_a <- make_overlap_atlas(arr1,
    ids = 1L, labels = "A1", hemis = "left"
  )
  atlas_b <- make_overlap_atlas(arr2,
    ids = 1L, labels = "B1", hemis = "left"
  )

  res <- atlas_overlap(atlas_a, atlas_b)

  expect_equal(nrow(res), 1L)
  expect_equal(res$n_overlap, 10L)
  expect_equal(res$n_atlas1, 20L)
  expect_equal(res$n_atlas2, 20L)
  expect_equal(res$dice, 0.5)
  expect_equal(res$jaccard, 1 / 3, tolerance = 1e-10)
  expect_equal(res$atlas1_label, "A1")
  expect_equal(res$atlas2_label, "B1")
})

test_that("atlas_overlap: min_overlap filtering works", {
  arr1 <- array(0L, dim = c(10, 5, 5))
  arr1[1:20] <- 1L
  arr1[21:40] <- 2L

  arr2 <- array(0L, dim = c(10, 5, 5))
  arr2[1:5] <- 1L   # 5 voxels overlap with region 1
  arr2[21:22] <- 2L  # 2 voxels overlap with region 2

  atlas_a <- make_overlap_atlas(arr1,
    ids = c(1L, 2L), labels = c("A1", "A2"), hemis = c("left", "right")
  )
  atlas_b <- make_overlap_atlas(arr2,
    ids = c(1L, 2L), labels = c("B1", "B2"), hemis = c("left", "right")
  )

  # Without filtering: both pairs present
  res_all <- atlas_overlap(atlas_a, atlas_b, min_overlap = 0)
  expect_equal(nrow(res_all), 2L)

  # Filter for >= 3 overlap: only the 5-voxel overlap remains
  res_filtered <- atlas_overlap(atlas_a, atlas_b, min_overlap = 3)
  expect_equal(nrow(res_filtered), 1L)
  expect_equal(res_filtered$n_overlap, 5L)

  # Filter for >= 10: nothing remains

  res_none <- atlas_overlap(atlas_a, atlas_b, min_overlap = 10)
  expect_equal(nrow(res_none), 0L)
})

test_that("atlas_overlap: mismatched dimensions resampled when possible", {
  arr1 <- array(0L, dim = c(5, 5, 5))
  arr1[1:10] <- 1L
  arr2 <- array(0L, dim = c(6, 6, 6))
  arr2[1:10] <- 1L

  atlas_a <- make_overlap_atlas(arr1,
    ids = 1L, labels = "A", hemis = "left"
  )
  atlas_b <- make_overlap_atlas(arr2,
    ids = 1L, labels = "B", hemis = "left"
  )

  # neuroim2::resample can handle simple spatial mismatches,
  # so this should succeed rather than error
  res <- atlas_overlap(atlas_a, atlas_b)
  expect_s3_class(res, "tbl_df")
})

test_that("atlas_overlap: error when atlas volume is missing", {
  atlas_bad <- structure(list(
    name = "bad",
    atlas = "not_a_volume",
    ids = 1L,
    labels = "A",
    hemi = "left"
  ), class = "atlas")
  arr1 <- array(0L, dim = c(5, 5, 5))
  arr1[1:10] <- 1L
  atlas_a <- make_overlap_atlas(arr1,
    ids = 1L, labels = "A", hemis = "left"
  )

  expect_error(atlas_overlap(atlas_bad, atlas_a))
})

test_that("atlas_overlap: selecting specific metrics works", {
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:10] <- 1L

  atlas_a <- make_overlap_atlas(arr,
    ids = 1L, labels = "R1", hemis = "left"
  )

  # Only dice
  res_dice <- atlas_overlap(atlas_a, atlas_a, metrics = "dice")
  expect_true("dice" %in% names(res_dice))
  expect_false("jaccard" %in% names(res_dice))

  # Only jaccard
  res_jacc <- atlas_overlap(atlas_a, atlas_a, metrics = "jaccard")
  expect_true("jaccard" %in% names(res_jacc))
  expect_false("dice" %in% names(res_jacc))
})

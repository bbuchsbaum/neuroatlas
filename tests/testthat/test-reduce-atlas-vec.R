# Helper: build a tiny atlas + matching data for unit tests (no disk I/O)
make_toy_setup <- function(dims = c(5, 5, 5), n_time = 3) {
  sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

  # Atlas with two clusters: id 1 and id 2
  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 1L   # cluster 1: 8 voxels

  atlas_arr[4:5, 4:5, 4:5] <- 2L   # cluster 2: 8 voxels
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas_obj <- list(
    name = "toy",
    atlas = atlas_vol,
    ids = c(1L, 2L),
    labels = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi = c(NA, NA)
  )
  class(atlas_obj) <- c("toy", "atlas")

  # Full mask: all TRUE
  full_mask <- neuroim2::LogicalNeuroVol(array(TRUE, dim = dims), space = sp3)

  # 4D data: constant value per cluster for easy verification
  sp4 <- neuroim2::NeuroSpace(dim = c(dims, n_time), spacing = c(1, 1, 1))
  data_arr <- array(0, dim = c(dims, n_time))
  for (t in seq_len(n_time)) {
    data_arr[1:2, 1:2, 1:2, t] <- t        # cluster 1 = time index
    data_arr[4:5, 4:5, 4:5, t] <- t * 10   # cluster 2 = 10 * time index
  }
  data_vol <- neuroim2::NeuroVec(data_arr, sp4)

  list(atlas = atlas_obj, mask = full_mask, data_vol = data_vol,
       sp3 = sp3, dims = dims, n_time = n_time)
}

test_that("reduce_atlas_vec returns ClusteredNeuroVec with correct dims", {
  s <- make_toy_setup()
  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, s$mask)

  expect_s4_class(cvec, "ClusteredNeuroVec")
  expect_equal(dim(cvec)[1:3], s$dims)
  expect_equal(dim(cvec)[4], s$n_time)
})

test_that("reduce_atlas_vec produces correct per-parcel averages", {
  s <- make_toy_setup(n_time = 4)
  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, s$mask)
  ts_mat <- neuroim2::values(cvec)  # T x K

  expect_equal(ncol(ts_mat), 2)
  expect_equal(nrow(ts_mat), 4)

  # Cluster 1: each voxel has value = t  =>  mean = t

  expect_equal(ts_mat[, 1], 1:4)
  # Cluster 2: each voxel has value = 10*t  =>  mean = 10*t
  expect_equal(ts_mat[, 2], (1:4) * 10)
})

test_that("reduce_atlas_vec masks out clusters not in mask", {
  s <- make_toy_setup()

  # Mask that covers only cluster 1
  partial_mask_arr <- array(FALSE, dim = s$dims)
  partial_mask_arr[1:2, 1:2, 1:2] <- TRUE
  partial_mask <- neuroim2::LogicalNeuroVol(partial_mask_arr, space = s$sp3)

  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, partial_mask)
  ts_mat <- neuroim2::values(cvec)

  # Should still have 2 columns (all atlas IDs)
  expect_equal(ncol(ts_mat), 2)
  # Cluster 1 should have real values
  expect_equal(ts_mat[, 1], 1:3)
  # Cluster 2 should be NA
  expect_true(all(is.na(ts_mat[, 2])))
})

test_that("reduce_atlas_vec accepts numeric NeuroVol mask (coerced to logical)", {
  s <- make_toy_setup()
  # Numeric mask (non-zero = TRUE)
  num_mask <- neuroim2::NeuroVol(array(1, dim = s$dims), space = s$sp3)

  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, num_mask)
  expect_s4_class(cvec, "ClusteredNeuroVec")
})

test_that("reduce_atlas_vec errors on dimension mismatch", {
  s <- make_toy_setup()
  wrong_dims <- c(6, 5, 5, 3)
  wrong_sp <- neuroim2::NeuroSpace(dim = wrong_dims, spacing = c(1, 1, 1))
  wrong_vol <- neuroim2::NeuroVec(array(0, dim = wrong_dims), wrong_sp)

  expect_error(reduce_atlas_vec(s$atlas, wrong_vol, s$mask), "dimensions")
})

test_that("reduce_atlas_vec errors when data_vol is not NeuroVec", {
  s <- make_toy_setup()
  vol3d <- neuroim2::NeuroVol(array(0, dim = s$dims), space = s$sp3)
  expect_error(reduce_atlas_vec(s$atlas, vol3d, s$mask), "NeuroVec")
})

test_that("reduce_atlas_vec handles non-contiguous atlas IDs", {
  dims <- c(5, 5, 5)
  sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

  # Non-contiguous IDs: 10, 50
  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 10L
  atlas_arr[4:5, 4:5, 4:5] <- 50L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas_obj <- list(
    name = "noncontig",
    atlas = atlas_vol,
    ids = c(10L, 50L),
    labels = c("RegionA", "RegionB"),
    orig_labels = c("RegionA", "RegionB"),
    hemi = c(NA, NA)
  )
  class(atlas_obj) <- c("noncontig", "atlas")

  mask <- neuroim2::LogicalNeuroVol(array(TRUE, dim = dims), space = sp3)

  sp4 <- neuroim2::NeuroSpace(dim = c(dims, 2), spacing = c(1, 1, 1))
  data_arr <- array(0, dim = c(dims, 2))
  data_arr[1:2, 1:2, 1:2, ] <- 5
  data_arr[4:5, 4:5, 4:5, ] <- 9
  data_vol <- neuroim2::NeuroVec(data_arr, sp4)

  cvec <- reduce_atlas_vec(atlas_obj, data_vol, mask)
  ts_mat <- neuroim2::values(cvec)

  expect_equal(ncol(ts_mat), 2)
  expect_equal(ts_mat[1, 1], 5)
  expect_equal(ts_mat[1, 2], 9)
})

test_that("reduce_atlas_vec returns all-NA when no mask voxels overlap atlas", {
  s <- make_toy_setup()

  # Mask covering only the center where no atlas voxels exist
  empty_mask_arr <- array(FALSE, dim = s$dims)
  empty_mask_arr[3, 3, 3] <- TRUE
  empty_mask <- neuroim2::LogicalNeuroVol(empty_mask_arr, space = s$sp3)

  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, empty_mask)
  ts_mat <- neuroim2::values(cvec)

  expect_equal(ncol(ts_mat), 2)
  expect_true(all(is.na(ts_mat)))
})

test_that("reduce_atlas_vec supports custom stat_func", {
  s <- make_toy_setup(n_time = 2)

  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, s$mask, stat_func = sum)
  ts_mat <- neuroim2::values(cvec)

  # Cluster 1: 8 voxels, each value = t  =>  sum = 8*t
  expect_equal(ts_mat[1, 1], 8)
  expect_equal(ts_mat[2, 1], 16)
  # Cluster 2: 8 voxels, each value = 10*t  =>  sum = 80*t
  expect_equal(ts_mat[1, 2], 80)
  expect_equal(ts_mat[2, 2], 160)
})

test_that("reduce_atlas_vec works with dilate = TRUE on toy data", {
  s <- make_toy_setup()

  # Dilate should still produce a valid ClusteredNeuroVec
  cvec <- reduce_atlas_vec(s$atlas, s$data_vol, s$mask,
                           dilate = TRUE, radius = 2, maxn = 5)
  expect_s4_class(cvec, "ClusteredNeuroVec")
  expect_equal(dim(cvec)[4], s$n_time)
})

test_that("reduce_atlas_vec works on real atlas", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  sp3 <- neuroim2::space(atl$atlas)
  dims3 <- dim(atl$atlas)

  sp4 <- neuroim2::NeuroSpace(dim = c(dims3, 5), spacing = neuroim2::spacing(sp3))
  data_vol <- neuroim2::NeuroVec(array(rnorm(prod(dims3) * 5), dim = c(dims3, 5)), sp4)

  # Use atlas voxels themselves as mask
  if (inherits(atl$atlas, "ClusteredNeuroVol")) {
    mask <- atl$atlas@mask
  } else {
    mask <- neuroim2::LogicalNeuroVol(atl$atlas > 0, space = sp3)
  }

  cvec <- reduce_atlas_vec(atl, data_vol, mask)
  expect_s4_class(cvec, "ClusteredNeuroVec")
  expect_equal(dim(cvec)[4], 5)
  expect_equal(ncol(neuroim2::values(cvec)), length(atl$ids))
})

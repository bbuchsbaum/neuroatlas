make_toy_atlas <- function() {
  dims <- c(5, 5, 5)
  space <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0)
  )
  arr <- array(0, dim = dims)
  arr[2, 3, 3] <- 1
  arr[5, 5, 5] <- 2
  vol <- neuroim2::NeuroVol(arr, space = space)

  atlas <- list(
    name = "toy",
    atlas = vol,
    ids = c(1L, 2L),
    labels = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi = c(NA, NA),
    network = NULL
  )
  class(atlas) <- c("toy", "atlas")
  atlas
}

test_that("dilate_atlas fills unlabeled voxels inside the mask", {
  atlas <- make_toy_atlas()
  mask <- neuroim2::NeuroVol(
    array(1, dim = dim(atlas$atlas)),
    space = neuroim2::space(atlas$atlas)
  )

  dilated <- dilate_atlas(atlas, mask, radius = 2, maxn = 5)
  dense <- neuroim2::as.dense(dilated$atlas)

  expect_equal(dense[3, 3, 3], 1)
  expect_setequal(unique(as.vector(dense[dense != 0])), atlas$ids)
  expect_equal(dim(dense), dim(atlas$atlas))
})

test_that("dilate_atlas radius bounds how far parcels grow", {
  dims <- c(7, 7, 7)
  space <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0)
  )
  arr <- array(0, dim = dims)
  arr[1, 1, 1] <- 1L  # a single parcel in one corner
  vol <- neuroim2::NeuroVol(arr, space = space)
  atlas <- list(
    name = "toy", atlas = vol, ids = 1L, labels = "A",
    orig_labels = "A", hemi = NA, network = NULL
  )
  class(atlas) <- c("toy", "atlas")

  mask_data <- array(0, dim = dims)
  mask_data[1, 1, 1] <- 1  # the parcel itself
  mask_data[2, 1, 1] <- 1  # near candidate: 1 voxel from the parcel
  mask_data[7, 7, 7] <- 1  # far candidate: ~10.4 voxels from the parcel
  mask <- neuroim2::NeuroVol(mask_data, space = space)

  small <- neuroim2::as.dense(
    suppressWarnings(dilate_atlas(atlas, mask, radius = 2, maxn = 5))$atlas
  )
  expect_equal(small[2, 1, 1], 1)  # near voxel is absorbed
  expect_equal(small[7, 7, 7], 0)  # far voxel is left unassigned by the guard

  large <- neuroim2::as.dense(
    suppressWarnings(dilate_atlas(atlas, mask, radius = 20, maxn = 5))$atlas
  )
  expect_equal(large[2, 1, 1], 1)
  expect_equal(large[7, 7, 7], 1)  # a large radius now reaches the far voxel
})

# Two parcels with NON-contiguous ids (1 and 5) so a broken neighbour->label
# mapping (or transposed neighbour matrix) would assign the wrong label rather
# than silently passing as it could with a single label.
make_two_parcel_atlas <- function(dims, pos_a, pos_b) {
  space <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))
  arr <- array(0, dim = dims)
  arr[pos_a[1], pos_a[2], pos_a[3]] <- 1L
  arr[pos_b[1], pos_b[2], pos_b[3]] <- 5L
  atlas <- list(
    name = "toy", atlas = neuroim2::NeuroVol(arr, space = space),
    ids = c(1L, 5L), labels = c("A", "B"), orig_labels = c("A", "B"),
    hemi = c(NA, NA), network = NULL
  )
  class(atlas) <- c("toy", "atlas")
  atlas
}

test_that("dilate_atlas assigns each voxel to its nearest parcel", {
  # A at (1,1,1), B at (9,1,1); candidates lie on the line between them.
  atlas <- make_two_parcel_atlas(c(9, 9, 9), c(1, 1, 1), c(9, 1, 1))

  mask_data <- array(0, dim = c(9, 9, 9))
  mask_data[1, 1, 1] <- 1                # parcel A
  mask_data[9, 1, 1] <- 1                # parcel B
  mask_data[2, 1, 1] <- 1                # 1 vox from A, 7 from B -> A (1)
  mask_data[8, 1, 1] <- 1                # 7 vox from A, 1 from B -> B (5)
  mask <- neuroim2::NeuroVol(mask_data, space = neuroim2::space(atlas$atlas))

  d <- neuroim2::as.dense(dilate_atlas(atlas, mask, radius = 12, maxn = 50)$atlas)
  expect_equal(d[2, 1, 1], 1)  # nearest parcel is A
  expect_equal(d[8, 1, 1], 5)  # nearest parcel is B (would be 1 if labels mis-mapped)
})

test_that("dilate_atlas handles maxn = 1 (single-neighbour search)", {
  atlas <- make_two_parcel_atlas(c(9, 9, 9), c(1, 1, 1), c(9, 1, 1))

  mask_data <- array(0, dim = c(9, 9, 9))
  mask_data[1, 1, 1] <- 1
  mask_data[9, 1, 1] <- 1
  mask_data[2, 1, 1] <- 1
  mask_data[8, 1, 1] <- 1
  mask <- neuroim2::NeuroVol(mask_data, space = neuroim2::space(atlas$atlas))

  d <- neuroim2::as.dense(dilate_atlas(atlas, mask, radius = 12, maxn = 1)$atlas)
  expect_equal(d[2, 1, 1], 1)
  expect_equal(d[8, 1, 1], 5)
})

test_that("dilate_atlas accumulates correctly across chunk boundaries", {
  # 22 * 22 * 23 = 11132 candidate voxels > the internal chunk_size of 10000,
  # so dilation runs through the multi-chunk accumulation path. Parcels sit at
  # the lowest and highest linear indices so labelled output lands in both chunks.
  dims <- c(22, 22, 23)
  atlas <- make_two_parcel_atlas(dims, c(1, 1, 1), dims)
  mask <- neuroim2::NeuroVol(array(1, dim = dims),
                             space = neuroim2::space(atlas$atlas))

  d <- neuroim2::as.dense(dilate_atlas(atlas, mask, radius = 100, maxn = 50)$atlas)
  expect_equal(dim(d), dims)
  expect_equal(sum(d != 0), prod(dims))                   # radius covers all voxels
  expect_setequal(unique(as.vector(d[d != 0])), c(1, 5))  # only the original ids
})

test_that("dilate_atlas respects mask holes", {
  atlas <- make_toy_atlas()
  mask_data <- array(1, dim = dim(atlas$atlas))
  mask_data[1, 1, 1] <- 0
  mask <- neuroim2::NeuroVol(
    mask_data,
    space = neuroim2::space(atlas$atlas)
  )

  dilated <- dilate_atlas(atlas, mask, radius = 2, maxn = 5)
  dense <- neuroim2::as.dense(dilated$atlas)

  expect_equal(dense[1, 1, 1], 0)
  expect_equal(sum(dense != 0 & mask_data == 0), 0)
})

test_that("dilate_atlas returns input when no dilation is possible", {
  atlas <- make_toy_atlas()
  atlas_data <- atlas$atlas@.Data
  mask_data <- array(as.numeric(atlas_data != 0), dim = dim(atlas$atlas))
  mask <- neuroim2::NeuroVol(
    mask_data,
    space = neuroim2::space(atlas$atlas)
  )

  dilated <- dilate_atlas(atlas, mask, radius = 2, maxn = 5)
  expect_identical(dilated, atlas)
})

test_that("dilate_atlas works on Glasser atlas", {
  skip_on_cran()  # Skip on CRAN for demonstration purposes
  
  # Load the Glasser atlas
  gl <- suppressWarnings(get_glasser_atlas())
  
  # Create a simple mask that's just the atlas itself
  # This tests that dilate_atlas handles the case where there's nothing to dilate
  if (inherits(gl$atlas, "ClusteredNeuroVol")) {
    # Use the mask from the ClusteredNeuroVol
    mask <- gl$atlas@mask
  } else {
    # Create mask from atlas
    mask <- neuroim2::LogicalNeuroVol(gl$atlas > 0, space = neuroim2::space(gl$atlas))
  } 
  # Attempt dilation with small parameters just for testing
  dilated <- dilate_atlas(atlas = gl, mask = mask, radius = 2, maxn = 20)
  
  # Check that the result is an atlas object like the input
  expect_true(inherits(dilated, "atlas"))
  expect_true(inherits(dilated, "glasser"))
  
  # Check that the atlas slot contains a ClusteredNeuroVol
  expect_true(inherits(dilated$atlas, "ClusteredNeuroVol"))
  
  # Check dimensions are preserved
  expect_equal(dim(dilated$atlas), dim(gl$atlas))
})

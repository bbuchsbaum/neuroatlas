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

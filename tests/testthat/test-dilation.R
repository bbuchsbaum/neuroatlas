test_that("dilate_atlas works on Glasser atlas", {
  skip_on_cran()  # Skip on CRAN for demonstration purposes
  
  # Load the Glasser atlas
  gl <- get_glasser_atlas()
  
  # Create a simple binary mask (all voxels of the Glasser volume)
  # Note: We convert the Glasser atlas volume to a logical mask
  mask <- as.logical(neuroim2::as.dense(gl$atlas)) 
  # Attempt dilation with small parameters just for testing
  dilated <- dilate_atlas(atlas = gl, mask = mask, radius = 2, maxn = 20)
  
  # Check that the result is a ClusteredNeuroVol
  expect_true(inherits(dilated, "ClusteredNeuroVol"))
  # Check that the dilated object still has no missing label_map entries
  expect_true(length(setdiff(unique(dilated), as.numeric(names(dilated@label_map)))) == 0)
})

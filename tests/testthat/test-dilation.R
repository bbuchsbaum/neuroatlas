test_that("dilate_atlas works on Glasser atlas", {
  skip_on_cran()  # Skip on CRAN for demonstration purposes
  
  # Load the Glasser atlas
  gl <- get_glasser_atlas()
  
  # Create a simple binary mask from the atlas
  # We want a NeuroVol mask that covers the entire brain space
  mask <- neuroim2::NeuroVol(
    array(1, dim = dim(gl$atlas)),  # All ones - full brain mask
    space = neuroim2::space(gl$atlas)
  ) 
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

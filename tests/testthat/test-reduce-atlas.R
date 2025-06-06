test_that("reduce_atlas computes summary statistics with default format", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  vol <- neuroim2::NeuroVol(array(rnorm(prod(dim(atl$atlas))), dim=dim(atl$atlas)),
                             space = neuroim2::space(atl$atlas))
  
  # Default should be long format for NeuroVol
  stats <- reduce_atlas(atl, vol, mean)
  
  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), 2)  # region, value
  expect_equal(nrow(stats), length(atl$ids))
  expect_equal(colnames(stats), c("region", "value"))
  expect_equal(stats$region, atl$orig_labels)
})

test_that("reduce_atlas names columns for matrix output", {
  skip_on_cran()
  skip("NeuroVec creation issue - skipping for now")
  
  atl <- get_aseg_atlas()
  # Create two volumes and concatenate them
  vol1 <- neuroim2::NeuroVol(array(rnorm(prod(dim(atl$atlas))), dim=dim(atl$atlas)),
                              space = neuroim2::space(atl$atlas))
  vol2 <- neuroim2::NeuroVol(array(rnorm(prod(dim(atl$atlas))), dim=dim(atl$atlas)),
                              space = neuroim2::space(atl$atlas))
  vec <- neuroim2::concat(vol1, vol2, along=4)
  stats <- reduce_atlas(atl, vec, mean)

  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 2)
  expect_equal(colnames(stats), c("time", atl$orig_labels))
})


test_that("reduce_atlas errors with mismatched dimensions", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  wrong_dim <- dim(atl$atlas) + c(1, 0, 0)
  new_space <- neuroim2::NeuroSpace(dim = wrong_dim,
                                    spacing = neuroim2::spacing(neuroim2::space(atl$atlas)))
  vol <- neuroim2::NeuroVol(array(0, dim = wrong_dim), space = new_space)
  expect_error(reduce_atlas(atl, vol, mean), "dimensions")
})

test_that("reduce_atlas works on Glasser atlas", {
  skip_on_cran()
  gl <- get_glasser_atlas()
  vol <- neuroim2::NeuroVol(array(rnorm(prod(dim(gl$atlas))), dim=dim(gl$atlas)),
                             space = neuroim2::space(gl$atlas))
  
  # Default is long format for NeuroVol
  stats <- reduce_atlas(gl, vol, mean)

  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), 2)  # region, value columns
  expect_equal(nrow(stats), length(gl$ids))
  expect_equal(colnames(stats), c("region", "value"))
})

test_that("reduce_atlas supports wide format", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  vol <- neuroim2::NeuroVol(array(rnorm(prod(dim(atl$atlas))), dim=dim(atl$atlas)),
                             space = neuroim2::space(atl$atlas))
  
  # Explicitly request wide format
  stats <- reduce_atlas(atl, vol, mean, format = "wide")
  
  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), length(atl$ids))
  expect_equal(nrow(stats), 1)
  expect_equal(colnames(stats), atl$orig_labels)
})

test_that("reduce_atlas supports long format for NeuroVec", {
  skip_on_cran()
  skip("NeuroVec creation issue - skipping for now")
  
  atl <- get_aseg_atlas()
  # Would create NeuroVec here
  # vec <- ...
  # stats <- reduce_atlas(atl, vec, mean, format = "long")
  
  # expect_s3_class(stats, "tbl_df")
  # expect_equal(ncol(stats), 3)  # time, region, value
  # expect_equal(colnames(stats), c("time", "region", "value"))
})

# test_that("reduce_atlas works on surface atlas", {
#   skip_on_cran()
#   atl <- get_schaefer_surfatlas(parcels = "100", networks = "7", surf = "inflated")
#   nvert <- length(atl$lh_atlas@data) + length(atl$rh_atlas@data)
#   dat <- rnorm(nvert)
#   stats <- reduce_atlas(atl, dat, mean)
#   expect_s3_class(stats, "tbl_df")
#   expect_equal(ncol(stats), length(atl$ids))
#   expect_equal(nrow(stats), 1)
# })

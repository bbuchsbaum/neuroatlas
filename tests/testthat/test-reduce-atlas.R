context("reduce_atlas")

test_that("reduce_atlas computes summary statistics", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  vol <- neuroim2::NeuroVol(array(rnorm(prod(dim(atl$atlas))), dim=dim(atl$atlas)),
                             space = neuroim2::space(atl$atlas))
  stats <- reduce_atlas(atl, vol, mean)

  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), length(atl$ids))
  expect_equal(nrow(stats), 1)
  expect_equal(colnames(stats), atl$orig_labels)
})

test_that("reduce_atlas names columns for matrix output", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  # Create 4D array with correct dimensions
  dims <- dim(atl$atlas)
  arr <- array(rnorm(prod(dims) * 2), dim=c(dims[1], dims[2], dims[3], 2))
  vec <- neuroim2::NeuroVec(arr, space = neuroim2::space(atl$atlas))
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
  stats <- reduce_atlas(gl, vol, mean)

  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), length(gl$ids))

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

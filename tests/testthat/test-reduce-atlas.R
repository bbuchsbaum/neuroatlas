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
  arr <- array(rnorm(prod(dim(atl$atlas)) * 2), dim=c(dim(atl$atlas), 2))
  vec <- neuroim2::NeuroVec(arr, space = neuroim2::space(atl$atlas))
  stats <- reduce_atlas(atl, vec, mean)

  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 2)
  expect_equal(colnames(stats), c("time", atl$orig_labels))
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

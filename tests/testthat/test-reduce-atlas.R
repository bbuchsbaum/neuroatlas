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
})

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

test_that("reduce_atlas works on surface atlas", {
  skip_on_cran()
  atl <- get_schaefer_surfatlas(parcels = "100", networks = "7", surf = "inflated")
  nvert <- length(atl$lh_atlas@data) + length(atl$rh_atlas@data)
  dat <- rnorm(nvert)
  stats <- reduce_atlas(atl, dat, mean)
  expect_s3_class(stats, "tbl_df")
  expect_equal(ncol(stats), length(atl$ids))
  expect_equal(nrow(stats), 1)
})

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

test_that("reduce_atlas errors with mismatched dimensions", {
  skip_on_cran()
  atl <- get_aseg_atlas()
  wrong_dim <- dim(atl$atlas) + c(1, 0, 0)
  new_space <- neuroim2::NeuroSpace(dim = wrong_dim,
                                    spacing = neuroim2::spacing(neuroim2::space(atl$atlas)))
  vol <- neuroim2::NeuroVol(array(0, dim = wrong_dim), space = new_space)
  expect_error(reduce_atlas(atl, vol, mean), "dimensions")
})

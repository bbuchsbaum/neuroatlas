context("merge_atlases")

test_that("merge_atlases combines ids and preserves dimensions", {
  skip_on_cran()
  a1 <- get_aseg_atlas()
  a2 <- get_aseg_atlas()
  merged <- merge_atlases(a1, a2)

  expect_true(inherits(merged, "atlas"))
  expect_equal(length(merged$ids), length(a1$ids) + length(a2$ids))
  expect_equal(dim(merged$atlas), dim(a1$atlas))
})

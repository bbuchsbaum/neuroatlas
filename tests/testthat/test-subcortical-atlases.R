test_that("subcortical atlas options expose expected metadata", {
  opts <- subcortical_atlas_options()
  expect_true(is.data.frame(opts))
  expect_true(all(c("id", "atlas", "default_space") %in% names(opts)))
  expect_true(all(c("cit168", "hcp_thalamus", "mdtb10", "hcp_hippamyg") %in%
                    opts$id))
})

test_that("aliases resolve to canonical subcortical atlas ids", {
  spec <- neuroatlas:::`.match_subcortical_spec`("Thalamus_HCP")
  expect_equal(spec$id, "hcp_thalamus")
})

test_that("invalid subcortical atlas names error clearly", {
  expect_error(
    get_subcortical_atlas("not_real_atlas"),
    "Unknown subcortical atlas"
  )
})

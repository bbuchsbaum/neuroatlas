test_that("atlas provenance accessors expose artifacts and history", {
  atlas <- get_aseg_atlas()

  prov <- atlas_provenance(atlas)
  expect_s3_class(prov, "atlas_provenance")
  expect_s3_class(prov$ref, "atlas_ref")
  expect_s3_class(prov$artifacts, "tbl_df")
  expect_s3_class(prov$history, "tbl_df")

  expect_gte(nrow(prov$artifacts), 1)
  expect_gte(nrow(prov$history), 1)
  expect_true(all(c("role", "source_name", "template_space") %in%
                    names(prov$artifacts)))
  expect_true(all(c("step", "action", "to_template_space") %in%
                    names(prov$history)))
  expect_equal(prov$history$action[[1]], "load")
})


test_that("legacy atlases fall back to empty artifact/history tables", {
  legacy <- list(
    name = "LegacyAtlas",
    ids = 1L,
    labels = "ROI1",
    hemi = NA_character_
  )
  class(legacy) <- c("legacy", "atlas")

  expect_equal(nrow(atlas_artifacts(legacy)), 0)
  expect_equal(nrow(atlas_history(legacy)), 0)
})


test_that("subset operations preserve provenance artifacts and append history", {
  atlas <- get_aseg_atlas()
  filtered <- filter_atlas(atlas, hemi == "left")

  expect_equal(nrow(atlas_artifacts(filtered)), nrow(atlas_artifacts(atlas)))
  expect_gt(nrow(atlas_history(filtered)), nrow(atlas_history(atlas)))
  expect_equal(tail(atlas_history(filtered)$action, 1), "subset")
  expect_equal(
    atlas_history(filtered)$step,
    seq_len(nrow(atlas_history(filtered)))
  )
})

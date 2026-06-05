visfatlas_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      testthat::skip(paste("visfAtlas download unavailable:", conditionMessage(e)))
    }
  )
}

test_that("visfAtlas label table is faithful to the FSL specification", {
  labels <- neuroatlas:::.visfatlas_labels()

  expect_s3_class(labels, "tbl_df")
  expect_equal(nrow(labels), 33L)
  expect_equal(labels$id, seq_len(33L))
  # lh has 17 regions (incl. two character regions), rh has 16 (incl. TOS).
  expect_equal(sum(labels$hemi == "left"), 17L)
  expect_equal(sum(labels$hemi == "right"), 16L)
  # Early visual areas present in both hemispheres.
  expect_true(all(c(
    "lh_v1d_retinotopic", "lh_v1v_retinotopic",
    "rh_v3d_retinotopic", "rh_v3v_retinotopic"
  ) %in% labels$label))
  # Source typo preserved in orig_label, corrected in label.
  expect_true("lh_IOS_haracters" %in% labels$orig_label)
  expect_true("lh_IOS_characters" %in% labels$label)
  expect_true(all(c("retinotopic", "faces", "bodies", "places", "motion") %in%
                    labels$category))
})

test_that("visfAtlas is registered for get_atlas dispatch", {
  specs <- list_atlases()
  expect_true("visfatlas" %in% specs$id)
  expect_s3_class(find_atlas_spec("rosenke"), "atlas_spec")
})

test_that("visfAtlas loads as a standard volumetric atlas", {
  skip_on_cran()

  atlas <- visfatlas_or_skip(get_visfatlas())

  expect_s3_class(atlas, "visfatlas")
  expect_s3_class(atlas, "volatlas")
  expect_s3_class(atlas, "atlas")
  expect_equal(length(atlas$ids), 33L)
  expect_false("hV4" %in% atlas$labels) # visfAtlas has no hV4
  expect_true(all(c("left", "right") %in% atlas$hemi))
  expect_equal(atlas_ref(atlas)$family, "visfatlas")
  expect_equal(nrow(atlas_artifacts(atlas)), 2L)

  # get_roi must work on a ClusteredNeuroVol-backed atlas.
  roi <- get_roi(atlas, label = "lh_v1d_retinotopic")
  expect_s4_class(roi[[1]], "ROIVol")
  expect_gt(nrow(neuroim2::coords(roi[[1]])), 0L)
})

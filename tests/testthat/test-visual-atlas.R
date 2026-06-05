visual_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      testthat::skip(paste("Julich/visual atlas unavailable:", conditionMessage(e)))
    }
  )
}

test_that("Julich visual label mapping recognises V1-V5", {
  labs <- c(
    "GM Visual cortex V1 BA17 L", "GM Visual cortex V2 BA18 R",
    "GM Visual cortex V3V L", "GM Visual cortex V4 R",
    "GM Visual cortex V5 L", "GM Premotor cortex BA6 L"
  )
  area <- neuroatlas:::.julich_visual_area(labs)
  expect_equal(area, c("V1", "V2", "V3", "V4", "V5", NA_character_))

  hemi <- neuroatlas:::.julich_label_hemi(labs)
  expect_equal(hemi, c("left", "right", "left", "right", "left", "left"))
})

test_that("visual atlas is registered for get_atlas dispatch", {
  specs <- list_atlases()
  expect_true("visual" %in% specs$id)
  expect_s3_class(find_atlas_spec("early_visual"), "atlas_spec")
})

test_that("get_visual_atlas extracts V1-V5 per hemisphere from Julich", {
  skip_on_cran()

  atlas <- visual_or_skip(get_visual_atlas())

  expect_s3_class(atlas, "visual")
  expect_s3_class(atlas, "volatlas")
  expect_true(all(atlas$labels %in% c("V1", "V2", "V3", "V4", "V5")))
  expect_true(all(c("V1", "V2", "V3", "V4") %in% atlas$labels))
  expect_true(all(c("left", "right") %in% atlas$hemi))
  expect_equal(atlas_ref(atlas)$family, "visual")

  # ROI extraction by label + hemisphere.
  roi <- get_roi(atlas, label = "V1", hemi = "left")
  expect_s4_class(roi[[1]], "ROIVol")
  expect_gt(nrow(neuroim2::coords(roi[[1]])), 0L)
})

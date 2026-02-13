test_that("space_transform_manifest exposes packaged registry", {
  reg <- space_transform_manifest()
  expect_true(is.data.frame(reg))
  expect_true(all(c(
    "from_space", "to_space", "transform_type", "backend",
    "confidence", "reversible", "data_files", "status", "notes"
  ) %in% names(reg)))
  expect_true(nrow(reg) > 0)
})


test_that("space_transform_manifest status filtering is stable", {
  planned <- space_transform_manifest(status = "planned")
  expect_true(is.data.frame(planned))
  expect_true(nrow(planned) > 0)
  expect_true(all(planned$status == "planned"))

  none <- space_transform_manifest(status = "not-a-status")
  expect_true(is.data.frame(none))
  expect_equal(nrow(none), 0L)
  expect_true(all(colnames(planned) %in% colnames(none)))
})


test_that("atlas_transform_plan returns direct route when available", {
  plan <- atlas_transform_plan("MNI305", "MNI152")
  expect_s3_class(plan, "atlas_transform_plan")
  expect_equal(plan$n_steps, 1L)
  expect_equal(plan$confidence, "exact")
  expect_equal(plan$status, "available")
  expect_equal(plan$steps$transform_type[[1]], "affine")
})


test_that("atlas_transform_plan returns identity for same-space transforms", {
  plan <- atlas_transform_plan("MNI152", "MNI152")
  expect_s3_class(plan, "atlas_transform_plan")
  expect_equal(plan$n_steps, 1L)
  expect_equal(plan$steps$transform_type[[1]], "identity")
  expect_equal(plan$confidence, "exact")
  expect_equal(length(plan$warnings), 0L)
})


test_that("atlas_transform_plan finds a two-hop route", {
  plan <- atlas_transform_plan("fsaverage5", "fsaverage6")
  expect_s3_class(plan, "atlas_transform_plan")
  expect_equal(plan$n_steps, 2L)
  expect_equal(plan$status, "available")
  expect_equal(plan$confidence, "exact")
  expect_equal(plan$steps$to_space[[1]], "fsaverage")
  expect_equal(plan$steps$from_space[[2]], "fsaverage")
})


test_that("atlas_transform_plan normalizes space aliases", {
  plan <- atlas_transform_plan("fsaverage", "fslr32k")
  expect_s3_class(plan, "atlas_transform_plan")
  expect_equal(plan$from_space, "fsaverage")
  expect_equal(plan$to_space, "fsLR_32k")
  expect_equal(plan$steps$to_space[[1]], "fsLR_32k")
  expect_equal(plan$status, "planned")
})


test_that("atlas_transform_plan surfaces advisory warnings", {
  plan_low_conf <- atlas_transform_plan("MNI152NLin2009cAsym", "fsaverage")
  expect_true(any(grepl("low-confidence", plan_low_conf$warnings)))
  expect_true(any(grepl("planned transform", plan_low_conf$warnings)))

  plan_vertex <- atlas_transform_plan(
    "fsaverage6", "fsaverage",
    data_type = "vertex"
  )
  expect_true(any(grepl("Nearest-neighbor", plan_vertex$warnings)))
})


test_that("atlas_transform_plan strict mode errors on missing route", {
  expect_error(
    atlas_transform_plan("NotASpace", "AlsoNotASpace", mode = "strict"),
    "No transform route found"
  )
})


test_that("atlas_transform_plan auto mode warns and returns NULL", {
  expect_warning(
    out <- atlas_transform_plan("NotASpace", "AlsoNotASpace", mode = "auto"),
    "No transform route found"
  )
  expect_null(out)
})


test_that("atlas_transform_manifest supports space scope", {
  manifest <- atlas_transform_manifest(scope = "space")
  expect_true(is.data.frame(manifest))
  expect_true(all(c("from_space", "to_space", "transform_type") %in% names(manifest)))
})


test_that("atlas_transform_manifest validates scope argument", {
  expect_error(
    atlas_transform_manifest(scope = "bad-scope"),
    "one of \"alignment\", \"space\""
  )
})


test_that("print.atlas_transform_plan returns invisibly", {
  plan <- atlas_transform_plan("MNI305", "MNI152")
  expect_invisible(print(plan))
})

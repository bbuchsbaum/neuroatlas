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


plan_route <- function(from, to, status = "available", confidence = "high",
                       artifact_id = paste(from, to, sep = "-"),
                       provider = "neuroatlas") {
  data.frame(
    from_space = from, to_space = to, transform_type = "nonlinear",
    backend = "ants", confidence = confidence, reversible = FALSE,
    data_files = NA_character_, status = status, notes = "test route",
    artifact_id = artifact_id, artifact_version = "v1", provider = provider,
    url = "https://example.org/releases/v1/transform.h5",
    sha256 = paste(rep("a", 64), collapse = ""), size_bytes = 1,
    format = "ants_h5", qualification = "passed", qa_url = NA_character_,
    license = "CC0", convention = "ants_image_pullback_ras",
    stringsAsFactors = FALSE
  )
}


test_that("execution planning prefers an available multi-hop route to planned direct", {
  registry <- rbind(
    plan_route("A", "D", status = "planned", artifact_id = "planned-direct"),
    plan_route("A", "B", artifact_id = "available-a-b"),
    plan_route("B", "C", artifact_id = "available-b-c"),
    plan_route("C", "D", artifact_id = "available-c-d")
  )
  local_mocked_bindings(.space_transform_registry = function() registry,
                        .package = "neuroatlas")

  plan <- atlas_transform_plan("A", "D", mode = "strict", available_only = TRUE)
  expect_identical(plan$status, "available")
  expect_identical(plan$n_steps, 3L)
  expect_identical(plan$steps$artifact_id,
                   c("available-a-b", "available-b-c", "available-c-d"))
})


test_that("planning ignores retired edges and has a deterministic tie break", {
  registry <- rbind(
    plan_route("A", "D", status = "retired", artifact_id = "retired"),
    plan_route("A", "B", artifact_id = "z-first"),
    plan_route("B", "D", artifact_id = "z-second"),
    plan_route("A", "C", artifact_id = "a-first"),
    plan_route("C", "D", artifact_id = "a-second")
  )
  local_mocked_bindings(.space_transform_registry = function() registry,
                        .package = "neuroatlas")

  first <- atlas_transform_plan("A", "D", mode = "strict")
  registry <- registry[c(5, 3, 1, 4, 2), ]
  second <- atlas_transform_plan("A", "D", mode = "strict")
  expect_identical(first$steps$artifact_id, c("a-first", "a-second"))
  expect_identical(second$steps$artifact_id, first$steps$artifact_id)
  expect_false(any(first$steps$status == "retired"))
})


test_that("planning terminates cyclic graphs and rejects planned-only execution", {
  registry <- rbind(
    plan_route("A", "B", artifact_id = "a-b"),
    plan_route("B", "A", artifact_id = "b-a"),
    plan_route("A", "C", artifact_id = "a-c"),
    plan_route("C", "D", artifact_id = "c-d")
  )
  local_mocked_bindings(.space_transform_registry = function() registry,
                        .package = "neuroatlas")
  expect_identical(atlas_transform_plan("A", "D", mode = "strict")$n_steps, 2L)

  registry$status[] <- "planned"
  expect_error(atlas_transform_plan("A", "D", mode = "strict",
                                    available_only = TRUE),
               "No transform route")
})


test_that("qualified MNI routes resolve to executable release artifacts", {
  spaces <- c("MNI152NLin6Asym", "MNI152NLin2009cAsym")
  for (from in spaces) {
    to <- setdiff(spaces, from)
    plan <- atlas_transform_plan(from, to, available_only = TRUE,
                                 provider = "neuroatlas", mode = "strict")
    expect_identical(plan$status, "available")
    expect_equal(plan$n_steps, 1L)
    expect_identical(plan$steps$qualification, "passed")
    expect_identical(plan$steps$convention, "ants_image_pullback_ras")
    expect_no_error(neuroatlas:::.validate_transform_artifact(plan$steps))
  }
})

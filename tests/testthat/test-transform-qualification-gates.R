artifact_root <- testthat::test_path("..", "..", "data-raw", "transform-artifacts-v1")
testthat::skip_if_not(dir.exists(artifact_root), "artifact build scripts are source-repository only")
artifact_root <- normalizePath(artifact_root)
source(file.path(artifact_root, "scripts", "qualification-gates.R"))

make_gate_policy <- function() {
  bounds <- setNames(lapply(qualification_required_metrics, function(metric) {
    if (metric %in% c("mi_cost", "cc_cost", "point_error_max_mm", "roundtrip_error_max_mm", "repeat_point_error_max_mm", "scalar_error_max", "nonpositive_jacobians", "nonfinite_jacobians")) list(max = 10) else list(min = 0)
  }), qualification_required_metrics)
  list(release_approval = list(approved = TRUE), calibration_sha256 = paste(rep("a", 64), collapse = ""), identity_baseline = setNames(rep(list(list(mi_cost = 2, cc_cost = 2, mask_dice = 0)), 4L), qualification_cell_keys), repeat_tolerances = list(mi_cost = .01, cc_cost = .01, mask_dice = .01, label_dice_min = .01, jacobian_min = .01), thresholds = list(cells = setNames(rep(list(bounds), 4L), qualification_cell_keys)))
}

make_gate_receipt <- function(path) list(path = path, bytes = unname(file.info(path)$size), sha256 = qualification_sha256(path))

make_gate_qa <- function(root, policy = make_gate_policy()) {
  forward <- file.path(root, "forward.h5"); inverse <- file.path(root, "inverse.h5")
  writeBin(as.raw(1:4), forward); writeBin(as.raw(5:8), inverse)
  provenance <- lapply(list(forward = forward, inverse = inverse), make_gate_receipt)
  build <- file.path(root, "build-provenance.json"); repeat_build <- file.path(root, "repeat-build-provenance.json")
  jsonlite::write_json(list(outputs = provenance), build, auto_unbox = TRUE)
  jsonlite::write_json(list(outputs = provenance), repeat_build, auto_unbox = TRUE)
  metrics <- setNames(as.list(rep(1, length(qualification_required_metrics))), qualification_required_metrics)
  metrics[c("probability_min", "jacobian_min", "mask_dice", "label_dice_min")] <- 1
  cells <- lapply(qualification_cell_keys, function(key) {
    parts <- strsplit(key, "_", fixed = TRUE)[[1L]]
    list(direction = parts[[1L]], resolution = as.numeric(sub("mm$", "", parts[[2L]])), metrics = metrics, raw = list(label_dice = list(per_label = list(`1` = 0.8))), invariants = setNames(as.list(rep(TRUE, length(qualification_required_invariants))), qualification_required_invariants))
  })
  review_path <- file.path(root, "review.txt"); writeLines("Synthetic review only", review_path)
  review <- list(passed = TRUE, reviewer = "independent reviewer", evidence_sha256 = qualification_sha256(review_path), evidence_receipt = make_gate_receipt(review_path))
  list(schema_version = 2, route_id = "synthetic", policy_sha256 = paste(rep("c", 64), collapse = ""), calibration_sha256 = policy$calibration_sha256, inputs = list(source = make_gate_receipt(forward)), candidates = provenance, repeat_candidates = provenance, build_provenance = make_gate_receipt(build), repeat_build_provenance = make_gate_receipt(repeat_build), software = list(neurotransform = list(remote_sha = paste(rep("a", 40), collapse = "")), niflowr = list(remote_sha = paste(rep("b", 40), collapse = ""))), cells = cells, repeat_cells = cells, reviews = list(visual = review, numerical_landmarks = review))
}

test_that("qualification gate accepts complete calibrated evidence", {
  root <- tempfile("qualification-gates-"); dir.create(root)
  qa <- make_gate_qa(root); policy <- make_gate_policy()
  expect_true(evaluate_qualification(qa, policy)$passed)
  expect_length(validate_qualification_bindings(qa), 0L)
})

test_that("qualification gate fails closed for missing direction and changed policy", {
  root <- tempfile("qualification-gates-"); dir.create(root)
  qa <- make_gate_qa(root); policy <- make_gate_policy()
  qa$cells <- qa$cells[-4L]
  verdict <- evaluate_qualification(qa, policy)
  expect_false(verdict$passed)
  expect_match(paste(verdict$failures, collapse = " "), "exactly four cells")
  root2 <- tempfile("qualification-gates-"); dir.create(root2)
  qa <- make_gate_qa(root2); policy$calibration_sha256 <- paste(rep("d", 64), collapse = "")
  expect_false(evaluate_qualification(qa, policy)$passed)
})

test_that("receipt binding detects tampered transform bytes", {
  root <- tempfile("qualification-gates-"); dir.create(root)
  qa <- make_gate_qa(root)
  writeBin(as.raw(9:12), file.path(root, "forward.h5"))
  failures <- validate_qualification_bindings(qa)
  expect_match(paste(failures, collapse = " "), "forward")
})

test_that("receipt checks reject a changed checksum", {
  root <- tempfile("qualification-gates-"); dir.create(root)
  path <- file.path(root, "evidence.txt")
  writeLines("first", path)
  receipt <- make_gate_receipt(path)
  writeLines("second", path)
  expect_false(qualification_receipt_matches(path, receipt))
})


test_that("invalid numeric bounds fail closed without an evaluation error", {
  root <- tempfile("qualification-gates-"); dir.create(root)
  qa <- make_gate_qa(root); policy <- make_gate_policy()
  key <- qualification_cell_keys[[1L]]
  policy$thresholds$cells[[key]]$mi_cost <- list(min = 0, max = NA_real_)
  expect_false(evaluate_qualification(qa, policy)$passed)
  policy$thresholds$cells[[key]]$mi_cost <- list(min = 2, max = 1)
  expect_false(evaluate_qualification(qa, policy)$passed)
})

test_that("identity equality and repeat QA drift cannot qualify", {
  root <- tempfile(); dir.create(root)
  policy <- make_gate_policy(); qa <- make_gate_qa(root, policy)
  key <- qualification_cell_keys[[1L]]
  for (metric in c("mi_cost", "cc_cost", "mask_dice")) {
    altered <- qa
    altered$cells[[1L]]$metrics[[metric]] <- policy$identity_baseline[[key]][[metric]]
    expect_false(evaluate_qualification(altered, policy)$passed)
  }
  qa$repeat_cells[[1L]]$metrics$mi_cost <- 1.1
  expect_false(evaluate_qualification(qa, policy)$passed)
})

test_that("missing numerical coverage and invalid repeat data fail closed", {
  root <- tempfile(); dir.create(root)
  policy <- make_gate_policy(); qa <- make_gate_qa(root, policy)
  for (invariant in c("landmark_coverage_complete", "repeat_support_complete",
                      "label_ids_preserved", "target_geometry_exact")) {
    altered <- qa
    altered$cells[[1L]]$invariants[[invariant]] <- FALSE
    expect_false(evaluate_qualification(altered, policy)$passed)
  }
  qa$repeat_cells[[1L]]$metrics$point_error_max_mm <- NA_real_
  expect_false(evaluate_qualification(qa, policy)$passed)
})

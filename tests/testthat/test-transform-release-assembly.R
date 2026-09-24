release_assembly_root <- testthat::test_path("..", "..", "data-raw", "transform-artifacts-v1")
skip_if_not(dir.exists(release_assembly_root), "artifact build scripts are source-repository only")
release_assembly_root <- normalizePath(release_assembly_root)
withr::local_envvar(c(NEUROATLAS_TRANSFORM_ARTIFACT_ROOT = release_assembly_root))
release_assembly_env <- new.env(parent = globalenv())
sys.source(file.path(release_assembly_root, "assemble-release.R"), release_assembly_env)

make_release_fixture <- function() {
  root <- tempfile("assembly-")
  candidate <- file.path(root, "candidate")
  dir.create(file.path(candidate, "visual"), recursive = TRUE)
  put <- function(name, value) {
    path <- file.path(candidate, name)
    writeLines(value, path)
    release_assembly_env$release_receipt(path)
  }
  write_json <- release_assembly_env$write_json
  receipts <- list(forward = put("forward.h5", "candidate forward"),
                   inverse = put("inverse.h5", "candidate inverse"))
  repeated <- list(forward = put("repeat-forward.h5", "distinct repeat forward"),
                   inverse = put("repeat-inverse.h5", "distinct repeat inverse"))
  input <- put("input.nii.gz", "frozen input")
  frozen <- input
  frozen$relpath <- frozen$path; frozen$path <- NULL
  write_json(list(inputs = list(source = frozen)), file.path(candidate, "calibration.json"))
  calibration_sha <- release_assembly_env$sha256_file(file.path(candidate, "calibration.json"))
  required <- release_assembly_env$qualification_required_metrics
  bounds <- setNames(lapply(required, function(name) list(min = 0, max = 10)), required)
  keys <- release_assembly_env$qualification_cell_keys
  policy <- list(release_approval = list(approved = TRUE),
    calibration_sha256 = calibration_sha,
    thresholds = list(cells = setNames(rep(list(bounds), 4), keys)),
    identity_baseline = setNames(rep(list(list(mi_cost = 2, cc_cost = 2, mask_dice = 0)), 4), keys), repeat_tolerances = list(mi_cost = .01, cc_cost = .01, mask_dice = .01, label_dice_min = .01, jacobian_min = .01))
  policy_path <- file.path(root, "qualification-policy.json")
  write_json(policy, policy_path)
  registration <- list(container = paste0("docker://ants@sha256:", strrep("c", 64)),
    preset = "precise", random_seed = 1L, threads = 8L,
    software_commits = list(neurotransform = strrep("a", 40), niflowr = strrep("b", 40)))
  software <- list(neurotransform = list(remote_sha = strrep("a", 40)),
    niflowr = list(remote_sha = strrep("b", 40)))
  side <- list(template = "synthetic", image = frozen, mask = frozen,
    qa_labels = frozen)
  route <- list(route_id = "synthetic", source_space = "A", target_space = "B",
    source = side, target = side, registration = registration,
    outputs = list(forward = "forward.h5", inverse = "inverse.h5"))
  write_json(list(artifact_release = "synthetic-v1", routes = list(route)),
    file.path(root, "routes.json"))
  inputs <- setNames(rep(list(input), 6L),
    c("source_image", "source_mask", "source_qa_labels", "target_image", "target_mask", "target_qa_labels"))
  provenance <- list(route_id = "synthetic", inputs = inputs,
    registration = registration, software = software, outputs = receipts)
  write_json(provenance, file.path(candidate, "build-provenance.json"))
  provenance$outputs <- repeated
  write_json(provenance, file.path(candidate, "repeat-build-provenance.json"))
  report <- put("report.html", "synthetic report")
  put("LICENSES.md", "synthetic fixture only")
  review_receipt <- put("review.txt", "synthetic review only")
  review <- list(passed = TRUE, reviewer = "synthetic reviewer",
    evidence_sha256 = review_receipt$sha256, evidence_receipt = review_receipt)
  panel <- put("visual/panel.html", "synthetic visual panel")
  panel$path <- "panel.html"
  write_json(list(qualitative_only = TRUE, files = list(panel)),
    file.path(candidate, "visual", "visual-qa.json"))
  get_receipt <- function(path) release_assembly_env$release_receipt(file.path(candidate, path))
  cells <- lapply(keys, function(key) {
    part <- strsplit(key, "_", fixed = TRUE)[[1]]
    list(direction = part[[1]], resolution = as.numeric(sub("mm", "", part[[2]])),
      metrics = setNames(as.list(rep(1, length(required))), required),
      invariants = setNames(as.list(rep(TRUE, length(release_assembly_env$qualification_required_invariants))),
                            release_assembly_env$qualification_required_invariants),
      raw = list(label_dice = list(per_label = list(`1` = 0.8))))
  })
  qa <- list(schema_version = 2, route_id = "synthetic",
    policy_sha256 = release_assembly_env$sha256_file(policy_path),
    calibration_sha256 = calibration_sha,
    inputs = list(source = input), candidates = receipts, repeat_candidates = repeated,
    build_provenance = get_receipt("build-provenance.json"),
    repeat_build_provenance = get_receipt("repeat-build-provenance.json"),
    report = report, visual_qa = get_receipt("visual/visual-qa.json"),
    software = list(neurotransform = list(remote_sha = strrep("a", 40)),
                    niflowr = list(remote_sha = strrep("b", 40))),
    cells = cells, repeat_cells = cells, reviews = list(visual = review, numerical_landmarks = review))
  write_json(qa, file.path(candidate, "qa.json"))
  list(root = root, candidate = candidate, release = file.path(root, "release"))
}

test_that("assembly validates real gates, distinct repeats and inverse directions", {
  f <- make_release_fixture()
  on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  release_assembly_env$assemble_release(f$candidate, f$release, artifact_dir = f$root)
  manifest <- jsonlite::read_json(file.path(f$release, "transform-artifacts-v1.json"))
  expect_identical(manifest$assets$forward$pair_id,
                   manifest$assets$inverse$pair_id)
  expect_identical(manifest$assets$forward$qualification_policy_sha256,
                   manifest$policy$sha256)
  expect_identical(manifest$assets$forward$from_space, "A")
  expect_identical(manifest$assets$inverse$from_space, "B")
  expect_identical(manifest$assets$inverse$to_space, "A")
  expect_match(manifest$assets$inverse$url, "releases/download/synthetic-v1/inverse.h5$")
  expect_true(file.exists(file.path(f$release, "visual", "panel.html")))
  expect_true(file.exists(file.path(f$release, "review.txt")))
  expect_false(file.exists(file.path(f$release, "visual", "visual", "panel.html")))
  expect_error(release_assembly_env$assemble_release(f$candidate, f$release,
    artifact_dir = f$root), "must not already exist")
})

test_that("assembly refuses changed bytes and missing scientific evidence", {
  for (kind in c("forward.h5", "calibration.json", "review.txt", "failed_gate")) {
    f <- make_release_fixture()
    on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
    if (kind == "failed_gate") {
      path <- file.path(f$candidate, "qa.json")
      qa <- jsonlite::read_json(path)
      qa$cells[[1]]$invariants$point_support_complete <- FALSE
      release_assembly_env$write_json(qa, path)
    } else if (kind == "calibration.json") {
      path <- file.path(f$candidate, kind)
      value <- jsonlite::read_json(path)
      value$extra <- "changed valid JSON"
      release_assembly_env$write_json(value, path)
    } else writeLines("tampered", file.path(f$candidate, kind))
    expect_error(release_assembly_env$assemble_release(f$candidate, f$release,
      artifact_dir = f$root))
    expect_false(dir.exists(f$release))
  }
})


test_that("assembly rejects a self-consistent receipt for an incorrect build input", {
  f <- make_release_fixture()
  on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  path <- file.path(f$candidate, "build-provenance.json")
  provenance <- jsonlite::read_json(path)
  provenance$inputs$source_image$sha256 <- strrep("d", 64)
  release_assembly_env$write_json(provenance, path)
  qa_path <- file.path(f$candidate, "qa.json")
  qa <- jsonlite::read_json(qa_path)
  qa$build_provenance <- release_assembly_env$release_receipt(path)
  release_assembly_env$write_json(qa, qa_path)
  expect_error(release_assembly_env$assemble_release(f$candidate, f$release,
    artifact_dir = f$root), "input differs")
  expect_false(dir.exists(f$release))
})

test_that("assembly retains checked failed-attempt evidence and rejects unsafe paths", {
  f <- make_release_fixture()
  on.exit(unlink(f$root, recursive = TRUE), add = TRUE)
  dir.create(file.path(f$candidate, "attempts", "failed-qualification01"), recursive = TRUE)
  evidence <- file.path(f$candidate, "attempts", "failed-qualification01", "stderr.log")
  writeLines("failed measurement receipt", evidence)
  qa_path <- file.path(f$candidate, "qa.json")
  qa <- jsonlite::read_json(qa_path, simplifyVector = FALSE)
  item <- release_assembly_env$release_receipt(evidence); item$path <- "attempts/failed-qualification01/stderr.log"
  qa$retained_attempts <- list(list(outcome = "failed", files = list(item)))
  release_assembly_env$write_json(qa, qa_path)
  release_assembly_env$assemble_release(f$candidate, f$release, artifact_dir = f$root)
  expect_true(file.exists(file.path(f$release, item$path)))
  manifest <- jsonlite::read_json(file.path(f$release, "transform-artifacts-v1.json"), simplifyVector = FALSE)
  expect_identical(manifest$retained_attempts[[1]]$outcome, "failed")

  bad <- make_release_fixture()
  on.exit(unlink(bad$root, recursive = TRUE), add = TRUE)
  qa_path <- file.path(bad$candidate, "qa.json"); qa <- jsonlite::read_json(qa_path, simplifyVector = FALSE)
  item <- release_assembly_env$release_receipt(file.path(bad$candidate, "report.html")); item$path <- "../report.html"
  qa$retained_attempts <- list(list(outcome = "rejected", files = list(item)))
  release_assembly_env$write_json(qa, qa_path)
  expect_error(release_assembly_env$assemble_release(bad$candidate, bad$release, artifact_dir = bad$root), "Retained attempt")
  expect_false(dir.exists(bad$release))

  changed <- make_release_fixture()
  on.exit(unlink(changed$root, recursive = TRUE), add = TRUE)
  evidence <- file.path(changed$candidate, "failed.log"); writeLines("original", evidence)
  qa_path <- file.path(changed$candidate, "qa.json"); qa <- jsonlite::read_json(qa_path, simplifyVector = FALSE)
  item <- release_assembly_env$release_receipt(evidence)
  qa$retained_attempts <- list(list(outcome = "rejected", files = list(item)))
  release_assembly_env$write_json(qa, qa_path); writeLines("changed", evidence)
  expect_error(release_assembly_env$assemble_release(changed$candidate, changed$release, artifact_dir = changed$root), "Retained attempt")
  expect_false(dir.exists(changed$release))
})

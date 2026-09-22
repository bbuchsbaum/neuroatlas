artifact_v1_path <- function(...) {
  normalizePath(file.path(testthat::test_path(), "..", "..", "data-raw", "transform-artifacts-v1", ...), mustWork = TRUE)
}

test_that("V1 transform inputs are fully pinned", {
  skip_if_not_installed("jsonlite")
  routes <- jsonlite::read_json(artifact_v1_path("routes.json"), simplifyVector = FALSE)
  expect_identical(routes$artifact_release, "transform-artifacts-v1")
  expect_identical(routes$status, "inputs_frozen_not_built")
  expect_length(routes$routes, 1L)

  route <- routes$routes[[1L]]
  expect_identical(route$source_space, "MNI152NLin6Asym")
  expect_identical(route$target_space, "MNI152NLin2009cAsym")
  expect_identical(route$registration$preset, "precise")
  expect_identical(route$registration$random_seed, 1L)
  expect_identical(route$registration$threads, 8L)
  expect_identical(route$registration$niflowr_development_ref,
                   "github:bbuchsbaum/niflowr@main")
  expect_match(route$registration$niflowr_last_observed_commit, "^[0-9a-f]{7,40}$")
  expect_match(route$registration$container, "@sha256:[0-9a-f]{64}$")

  receipts <- c(route$source[c("image", "mask", "qa_labels")], route$target[c("image", "mask", "qa_labels")])
  for (receipt in receipts) {
    expect_match(receipt$relpath, "^tpl-")
    expect_gt(receipt$bytes, 0)
    expect_match(receipt$sha256, "^[0-9a-f]{64}$")
  }
})

test_that("V1 qualification fails closed until calibrated thresholds are approved", {
  skip_if_not_installed("jsonlite")
  policy <- jsonlite::read_json(artifact_v1_path("qualification-policy.json"), simplifyVector = FALSE)
  expect_identical(policy$status, "draft_requires_benchmark_and_review")
  expect_false(policy$release_approval$approved)
  expect_null(policy$thresholds)
  expect_identical(policy$invariants$nonpositive_jacobians_allowed, 0L)
  expect_identical(policy$invariants$nonfinite_jacobians_allowed, 0L)
  expect_true(policy$evidence$visual_qa$required)
  expect_identical(policy$evidence$visual_qa$renderer, "neuroim2 registration-QC plots")
  expect_match(policy$evidence$software_provenance$release,
               "immutable source commits")
})

test_that("Nibi campaigns remain visibly unconfigured and separate smoke evidence", {
  production <- readLines(artifact_v1_path("campaign.nibi.toml"), warn = FALSE)
  smoke <- readLines(artifact_v1_path("campaign.nibi-smoke.toml"), warn = FALSE)
  expect_true(any(grepl("__NIBI_REMOTE_ROOT__", production, fixed = TRUE)))
  expect_true(any(grepl("require = \"verified\"", production, fixed = TRUE)))
  expect_true(any(grepl("__NIBI_SMOKE_REMOTE_ROOT__", smoke, fixed = TRUE)))
  expect_true(any(grepl("synthetic-testing-only", smoke, fixed = TRUE)))
  expect_true(any(grepl("tpl-synthetic_from-shifted", smoke, fixed = TRUE)))
  expect_true(any(grepl("module load hdf5/1.14.2", smoke, fixed = TRUE)))
  expect_true(any(grepl("R_LIBS_USER", smoke, fixed = TRUE)))
  expect_true(any(grepl("NEUROATLAS_TRANSFORM_WORK_ROOT", smoke, fixed = TRUE)))
})

test_that("V1 build scripts are syntactically valid R", {
  scripts <- c(
    artifact_v1_path("materialize-inputs.R"),
    artifact_v1_path("build.R"),
    artifact_v1_path("qualify.R"),
    artifact_v1_path("assemble-release.R"),
    artifact_v1_path("smoke.R"),
    artifact_v1_path("scripts", "common.R"),
    artifact_v1_path("scripts", "visual-qa.R"),
    artifact_v1_path("scripts", "validate-build-output.R"),
    artifact_v1_path("scripts", "validate-qualification-output.R"),
    artifact_v1_path("scripts", "validate-smoke-output.R")
  )
  for (script in scripts) expect_error(parse(script), NA)
})

test_that("campaign launchers resolve the synchronized artifact root", {
  launchers <- c(
    artifact_v1_path("scripts", "run-build.sh"),
    artifact_v1_path("scripts", "run-qualification.sh"),
    artifact_v1_path("scripts", "run-smoke.sh")
  )
  for (launcher in launchers) {
    lines <- readLines(launcher, warn = FALSE)
    expect_true(any(grepl("NEUROATLAS_TRANSFORM_ARTIFACT_ROOT", lines, fixed = TRUE)))
    expect_false(any(grepl("BASH_SOURCE", lines, fixed = TRUE)))
  }
  smoke_lines <- readLines(launchers[[3L]], warn = FALSE)
  expect_true(any(grepl("expected_smoke_sha256", smoke_lines, fixed = TRUE)))
  expect_true(any(grepl("expected_common_sha256", smoke_lines, fixed = TRUE)))
})

test_that("Nibi campaign materialization replaces only explicit sentinels", {
  script <- artifact_v1_path("materialize-nibi-campaign.R")
  fixture_dir <- tempfile()
  dir.create(fixture_dir)
  template <- file.path(fixture_dir, "campaign.nibi-smoke.toml")
  file.copy(artifact_v1_path("campaign.nibi-smoke.toml"), template)
  output <- file.path(fixture_dir, "campaign.nibi-smoke.generated.toml")
  result <- system2(
    "Rscript",
    c(script, template, output, "/remote-root", "/output-root"),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_identical(attr(result, "status"), NULL)
  contents <- readLines(output, warn = FALSE)
  expect_false(any(grepl("__NIBI_", contents, fixed = TRUE)))
  expect_true(any(grepl("remote_root = \"/remote-root\"", contents, fixed = TRUE)))
  expect_true(any(grepl("output_root = \"/output-root\"", contents, fixed = TRUE)))
})

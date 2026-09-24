entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
release_source_root <- Sys.getenv("NEUROATLAS_TRANSFORM_ARTIFACT_ROOT", unset = "")
if (!nzchar(release_source_root)) {
  release_source_root <- if (length(entry_file) == 1L) dirname(entry_file) else {
    file.path(getwd(), "data-raw", "transform-artifacts-v1")
  }
}
release_source_root <- normalizePath(release_source_root, mustWork = TRUE)
source(file.path(release_source_root, "scripts", "common.R"), local = TRUE)
source(file.path(release_source_root, "scripts", "qualification-gates.R"), local = TRUE)

release_receipt <- function(path) {
  receipt <- file_receipt(path)
  receipt$path <- basename(path)
  receipt
}

same_receipt <- function(x, y) {
  # Frozen input inventories have relpath, while measured receipts have path.
  if (is.list(y) && is.null(y$path)) y$path <- y$relpath
  qualification_is_receipt(x) && qualification_is_receipt(y) &&
    identical(as.numeric(x$bytes), as.numeric(y$bytes)) &&
    identical(x$sha256, y$sha256)
}

release_copy <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, copy.date = TRUE)) {
    stop("Could not copy release asset: ", from, call. = FALSE)
  }
  invisible(to)
}

release_route <- function(root, id) {
  routes <- read_json(file.path(root, "routes.json"))
  hit <- Filter(function(x) identical(x$route_id, id), routes$routes)
  if (length(hit) != 1L) stop("QA route_id is not defined by routes.json.")
  list(release = routes$artifact_release, route = hit[[1L]])
}

bind_provenance <- function(path, receipt, candidates, route) {
  if (!qualification_receipt_matches(path, receipt)) {
    stop("Build provenance receipt does not match file.")
  }
  provenance <- read_json(path)
  for (name in c("forward", "inverse")) {
    if (!same_receipt(provenance$outputs[[name]], candidates[[name]])) {
      stop("Build provenance is not bound to its ", name, " candidate.")
    }
  }
  if (!identical(provenance$route_id, route$route_id)) {
    stop("Build provenance names a different route.")
  }
  expected_inputs <- route_input_specs(route)
  for (name in names(expected_inputs)) {
    if (!same_receipt(provenance$inputs[[name]], expected_inputs[[name]])) {
      stop("Build provenance input differs from frozen route: ", name)
    }
  }
  for (name in c("container", "preset", "random_seed", "threads")) {
    if (is.null(route$registration[[name]]) ||
        !identical(provenance$registration[[name]], route$registration[[name]])) {
      stop("Build provenance registration differs from frozen route: ", name)
    }
  }
  for (package in names(route$registration$software_commits)) {
    if (!identical(provenance$software[[package]]$remote_sha,
                   route$registration$software_commits[[package]])) {
      stop("Build provenance uses an unexpected software revision: ", package)
    }
  }

}

check_inputs <- function(qa, calibration) {
  frozen <- read_json(calibration)$inputs
  if (!is.list(frozen) ||
      !identical(sort(names(qa$inputs)), sort(names(frozen)))) {
    stop("QA inputs do not match calibration input names.")
  }
  for (name in names(frozen)) {
    if (!same_receipt(qa$inputs[[name]], frozen[[name]])) {
      stop("QA input is not bound to calibration: ", name)
    }
  }
}

release_relative_path <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", path)
}

assemble_release <- function(candidate_dir, release_dir,
                             artifact_dir = release_source_root) {
  candidate_dir <- normalizePath(candidate_dir, mustWork = TRUE)
  artifact_dir <- normalizePath(artifact_dir, mustWork = TRUE)
  if (file.exists(release_dir)) stop("Release directory must not already exist.")
  qa_path <- file.path(candidate_dir, "qa.json")
  policy_path <- file.path(artifact_dir, "qualification-policy.json")
  calibration <- file.path(candidate_dir, "calibration.json")
  report_path <- file.path(candidate_dir, "report.html")
  visual_dir <- file.path(candidate_dir, "visual")
  visual_path <- file.path(visual_dir, "visual-qa.json")
  provenance_paths <- file.path(candidate_dir,
    c("build-provenance.json", "repeat-build-provenance.json"))
  required <- c(qa_path, policy_path, calibration, report_path, visual_path,
    provenance_paths, file.path(candidate_dir, "LICENSES.md"))
  if (any(!file.exists(required)) || any(file.info(required)$size < 1)) {
    stop("Candidate lacks required qualification evidence.")
  }
  qa <- read_json(qa_path)
  policy <- read_json(policy_path)
  if (!identical(qa$policy_sha256, sha256_file(policy_path))) {
    stop("QA does not bind policy bytes.")
  }
  if (!identical(qa$calibration_sha256, sha256_file(calibration)) ||
      !identical(policy$calibration_sha256, sha256_file(calibration))) {
    stop("QA and policy do not bind calibration bytes.")
  }
  verdict <- evaluate_qualification(qa, policy)
  if (!verdict$passed) stop(paste(verdict$failures, collapse = "; "))
  info <- release_route(artifact_dir, qa$route_id)
  route <- info$route
  filenames <- unlist(route$outputs[c("forward", "inverse")])
  if (length(filenames) != 2L ||
      !all(vapply(filenames, release_relative_path, logical(1))) ||
      any(basename(filenames) != filenames)) stop("Invalid route output names.")
  candidates <- setNames(as.list(file.path(candidate_dir, filenames)),
                         c("forward", "inverse"))
  for (name in names(candidates)) {
    if (!qualification_receipt_matches(candidates[[name]], qa$candidates[[name]])) {
      stop("Candidate H5 does not match QA receipt: ", name)
    }
    if (!qualification_is_receipt(qa$repeat_candidates[[name]])) {
      stop("Repeat candidate receipt is missing: ", name)
    }
  }
  bind_provenance(provenance_paths[[1]], qa$build_provenance, qa$candidates, info$route)
  bind_provenance(provenance_paths[[2]], qa$repeat_build_provenance,
                  qa$repeat_candidates, info$route)
  check_inputs(qa, calibration)
  if (!qualification_receipt_matches(report_path, qa$report) ||
      !qualification_receipt_matches(visual_path, qa$visual_qa)) {
    stop("Report or visual manifest is not bound to QA receipt.")
  }
  review_paths <- vapply(qa$reviews, function(review) {
    receipt <- review$evidence_receipt
    if (!qualification_is_receipt(receipt) ||
        !release_relative_path(receipt$path) ||
        !identical(review$evidence_sha256, receipt$sha256) ||
        !qualification_receipt_matches(file.path(candidate_dir, receipt$path), receipt)) {
      stop("Review evidence receipt does not match file.")
    }
    receipt$path
  }, character(1))
  retained_attempts <- if (is.null(qa$retained_attempts)) list() else qa$retained_attempts
  retained_paths <- character()
  if (!is.list(retained_attempts)) stop("Retained attempts must be a list.")
  for (attempt in retained_attempts) {
    if (!is.list(attempt) || !is.character(attempt$outcome) ||
        length(attempt$outcome) != 1L || !nzchar(attempt$outcome) ||
        !is.list(attempt$files) || !length(attempt$files)) {
      stop("Retained attempt lacks outcome or evidence receipts.")
    }
    for (evidence in attempt$files) {
      if (!qualification_is_receipt(evidence) || !release_relative_path(evidence$path) ||
          !qualification_receipt_matches(file.path(candidate_dir, evidence$path), evidence)) {
        stop("Retained attempt evidence receipt does not match file.")
      }
      retained_paths <- c(retained_paths, evidence$path)
    }
  }
  visual <- read_json(visual_path)
  if (!isTRUE(visual$qualitative_only) || !is.list(visual$files) ||
      !length(visual$files)) stop("Visual evidence is incomplete.")
  for (receipt in visual$files) {
    if (!qualification_is_receipt(receipt) ||
        !release_relative_path(receipt$path) ||
        !qualification_receipt_matches(file.path(visual_dir, receipt$path), receipt)) {
      stop("Visual evidence receipt does not match file.")
    }
  }
  # All checks precede the first output mutation.
  dir.create(release_dir, recursive = TRUE)
  for (path in c(unlist(candidates), required)) {
    release_copy(path, file.path(release_dir, basename(path)))
  }
  for (path in unique(review_paths)) {
    release_copy(file.path(candidate_dir, path), file.path(release_dir, path))
  }
  for (path in unique(retained_paths)) {
    release_copy(file.path(candidate_dir, path), file.path(release_dir, path))
  }
  for (path in list.files(visual_dir, recursive = TRUE, all.files = FALSE)) {
    release_copy(file.path(visual_dir, path), file.path(release_dir, "visual", path))
  }
  base <- paste0("https://github.com/bbuchsbaum/neuroatlas/releases/download/",
                 info$release, "/")
  assets <- lapply(names(candidates), function(name) {
    path <- file.path(release_dir, basename(candidates[[name]]))
    list(artifact_id = tools::file_path_sans_ext(basename(path)),
      artifact_version = info$release, pair_id = qa$route_id,
      provider = "neuroatlas", representation = "volume",
      transform_type = "nonlinear_warp", filename = basename(path),
      qualified_target_resolutions_mm = c(1L, 2L),
      qualification_policy_sha256 = qa$policy_sha256,
      from_space = if (name == "forward") route$source_space else route$target_space,
      to_space = if (name == "forward") route$target_space else route$source_space,
      url = paste0(base, basename(path)), sha256 = sha256_file(path),
      size_bytes = unname(file.info(path)$size), format = "ants_h5",
      convention = "ants_image_pullback_ras", qualification = "passed",
      qa_url = paste0(base, "qa.json"), license = paste0(base, "LICENSES.md"))
  })
  names(assets) <- names(candidates)
  write_json(list(schema_version = 2, artifact_release = info$release,
    route_id = qa$route_id, qa = release_receipt(qa_path),
    policy = release_receipt(policy_path),
    calibration = release_receipt(calibration), assets = assets,
    inputs = qa$inputs, registration = route$registration,
    software = qa$software,
    provenance = list(candidate = release_receipt(provenance_paths[[1L]]),
      repeat_build = release_receipt(provenance_paths[[2L]])),
    citations = list(source = route$source$references,
      target = route$target$references),
    retained_attempts = retained_attempts),
    file.path(release_dir, "transform-artifacts-v1.json"))
  paths <- sort(list.files(release_dir, recursive = TRUE, all.files = FALSE))
  checksums <- vapply(paths, function(path) {
    paste(sha256_file(file.path(release_dir, path)), path, sep = "  ")
  }, character(1))
  writeLines(checksums, file.path(release_dir, "SHA256SUMS"))
  invisible(normalizePath(release_dir))
}

if (sys.nframe() == 0L && !interactive()) {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) != 2L) {
    stop("Usage: Rscript assemble-release.R <qualified-candidate-dir> <release-dir>")
  }
  assemble_release(arguments[[1L]], arguments[[2L]])
}

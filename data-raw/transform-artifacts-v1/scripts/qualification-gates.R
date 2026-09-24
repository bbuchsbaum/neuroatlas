# Fail-closed qualification checks shared by the campaign validator and release
# assembler.  This file deliberately has no command-line side effects.

qualification_required_metrics <- c(
  "mi_cost", "cc_cost", "mask_dice", "label_dice_min", "point_error_max_mm",
  "roundtrip_error_max_mm", "repeat_point_error_max_mm", "scalar_error_max",
  "probability_min", "probability_max", "jacobian_min",
  "nonpositive_jacobians", "nonfinite_jacobians"
)
qualification_required_invariants <- c(
  "target_geometry_exact", "label_ids_preserved", "finite_outputs",
  "point_support_complete", "independent_reader_pass",
  "probability_channels_preserved", "label_oracle_agreement",
  "repeat_support_complete", "landmark_coverage_complete"
)
qualification_required_reviews <- c("visual", "numerical_landmarks")
qualification_cell_keys <- as.vector(outer(c("forward", "inverse"), c(1L, 2L),
  function(direction, resolution) paste0(direction, "_", resolution, "mm")
))

qualification_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

qualification_is_sha256 <- function(value) {
  is.character(value) && length(value) == 1L && grepl("^[0-9a-f]{64}$", value)
}

qualification_is_receipt <- function(receipt) {
  is.list(receipt) && is.character(receipt$path) && length(receipt$path) == 1L &&
    nzchar(receipt$path) && is.numeric(receipt$bytes) && length(receipt$bytes) == 1L &&
    is.finite(receipt$bytes) && receipt$bytes >= 0 &&
    qualification_is_sha256(receipt$sha256)
}

qualification_receipt_matches <- function(path, receipt) {
  if (!qualification_is_receipt(receipt) || !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  identical(as.numeric(file.info(path)$size), as.numeric(receipt$bytes)) &&
    identical(qualification_sha256(path), receipt$sha256)
}

qualification_valid_bounds <- function(bounds) {
  if (!is.list(bounds) || !length(bounds) ||
      !all(names(bounds) %in% c("min", "max"))) return(FALSE)
  finite_scalar <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!all(vapply(bounds, finite_scalar, logical(1)))) return(FALSE)
  is.null(bounds$min) || is.null(bounds$max) || bounds$min <= bounds$max
}

qualification_policy_errors <- function(policy) {
  errors <- character()
  if (!is.list(policy) || !isTRUE(policy$release_approval$approved)) {
    errors <- c(errors, "policy is not approved")
  }
  if (!qualification_is_sha256(policy$calibration_sha256)) {
    errors <- c(errors, "policy calibration_sha256 is absent or invalid")
  }
  cells <- policy$thresholds$cells
  if (!is.list(cells) || !identical(sort(names(cells)), sort(qualification_cell_keys))) {
    return(c(errors, "policy thresholds must define exactly four direction-resolution cells"))
  }
  for (key in qualification_cell_keys) {
    metrics <- cells[[key]]
    if (!is.list(metrics) || !all(qualification_required_metrics %in% names(metrics))) {
      errors <- c(errors, paste0("policy cell lacks required thresholds: ", key))
      next
    }
    for (metric in qualification_required_metrics) {
      bounds <- metrics[[metric]]
      valid_bound <- qualification_valid_bounds(bounds)
      if (!valid_bound) errors <- c(errors, paste0("policy threshold is invalid: ", key, "/", metric))
    }
  }
  if (!is.list(policy$identity_baseline) ||
      !identical(sort(names(policy$identity_baseline)), sort(qualification_cell_keys))) {
    errors <- c(errors, "policy must freeze all identity baselines")
  }
  if (!is.list(policy$repeat_tolerances) ||
      !all(c("mi_cost", "cc_cost", "mask_dice", "label_dice_min", "jacobian_min") %in% names(policy$repeat_tolerances)) ||
      any(!vapply(policy$repeat_tolerances, function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0, logical(1)))) {
    errors <- c(errors, "policy repeat tolerances are missing or invalid")
  }
  unique(errors)
}

evaluate_qualification <- function(qa, policy) {
  failures <- qualification_policy_errors(policy)
  fail <- function(message) failures <<- c(failures, message)
  if (!is.list(qa) || !identical(as.numeric(qa$schema_version), 2)) fail("qa schema_version must be 2")
  if (!is.character(qa$route_id) || length(qa$route_id) != 1L || !nzchar(qa$route_id)) fail("qa route_id is missing")
  if (!qualification_is_sha256(qa$policy_sha256)) fail("qa policy_sha256 is absent or invalid")
  if (!qualification_is_sha256(qa$calibration_sha256) || !identical(qa$calibration_sha256, policy$calibration_sha256)) {
    fail("qa calibration_sha256 does not match policy")
  }
  for (field in c("inputs", "candidates", "repeat_candidates")) {
    if (!is.list(qa[[field]]) || !length(qa[[field]]) || is.null(names(qa[[field]])) ||
        any(!nzchar(names(qa[[field]]))) || any(!vapply(qa[[field]], qualification_is_receipt, logical(1)))) {
      fail(paste0("qa ", field, " must be non-empty named receipts"))
    }
  }
  if (!is.list(qa$candidates) || !identical(sort(names(qa$candidates)), c("forward", "inverse"))) {
    fail("qa candidates must contain exactly forward and inverse receipts")
  }
  for (field in c("build_provenance", "repeat_build_provenance")) {
    if (!qualification_is_receipt(qa[[field]])) fail(paste0("qa ", field, " receipt is invalid"))
  }
  for (package in c("neurotransform", "niflowr")) {
    receipt <- qa$software[[package]]
    if (!is.list(receipt) || !is.character(receipt$remote_sha) || length(receipt$remote_sha) != 1L ||
        !grepl("^[0-9a-f]{40}$", receipt$remote_sha)) {
      fail(paste0("qa software receipt lacks immutable remote_sha: ", package))
    }
  }
  for (group in c("cells", "repeat_cells")) {
  cells <- qa[[group]]
  if (!is.list(cells) || length(cells) != 4L) {
    fail("qa must contain exactly four cells")
  } else {
    keys <- vapply(cells, function(cell) {
      if (!is.list(cell)) return(NA_character_)
      paste0(cell$direction, "_", cell$resolution, "mm")
    }, character(1))
    if (anyNA(keys) || !identical(sort(keys), sort(qualification_cell_keys))) fail("qa cells are missing or duplicated")
    for (index in seq_along(cells)) {
      cell <- cells[[index]]
      key <- keys[[index]]
      if (!is.list(cell$metrics) || !all(qualification_required_metrics %in% names(cell$metrics))) {
        fail(paste0("qa cell lacks required metrics: ", key)); next
      }
      if (!is.list(cell$invariants) || !all(qualification_required_invariants %in% names(cell$invariants))) {
        fail(paste0("qa cell lacks required invariants: ", key))
      } else if (any(!vapply(cell$invariants[qualification_required_invariants], isTRUE, logical(1)))) {
        fail(paste0("qa cell has failed invariant: ", key))
      }
      thresholds <- policy$thresholds$cells[[key]]
      for (metric in qualification_required_metrics) {
        value <- cell$metrics[[metric]]
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
          fail(paste0("qa metric is not finite: ", key, "/", metric)); next
        }
        bounds <- thresholds[[metric]]
        if (!qualification_valid_bounds(bounds) || (!is.null(bounds$min) && value < bounds$min) ||
            (!is.null(bounds$max) && value > bounds$max)) {
          fail(paste0("qa metric violates threshold: ", key, "/", metric))
        }
      }
      identity <- policy$identity_baseline[[key]]
      if (!is.list(identity) || !all(c("mi_cost", "cc_cost", "mask_dice") %in% names(identity)) ||
          !isTRUE(cell$metrics$mi_cost < identity$mi_cost) ||
          !isTRUE(cell$metrics$cc_cost < identity$cc_cost) ||
          !isTRUE(cell$metrics$mask_dice > identity$mask_dice)) {
        fail(paste0("qa must strictly improve identity: ", group, "/", key))
      }
      observed_labels <- unlist(cell$raw$label_dice$per_label, use.names = TRUE)
      if (!is.numeric(observed_labels) || !length(observed_labels) ||
          any(!is.finite(observed_labels)) || any(observed_labels < 0 | observed_labels > 1)) {
        fail(paste0("qa per-label concordance is invalid: ", key))
      }
    }
  }
  }
  if (length(qa$cells) == 4L && length(qa$repeat_cells) == 4L) {
    for (cell in qa$cells) {
      key <- paste0(cell$direction, "_", cell$resolution, "mm")
      other <- Filter(function(x) identical(x$direction, cell$direction) &&
        identical(as.numeric(x$resolution), as.numeric(cell$resolution)), qa$repeat_cells)
      if (length(other) != 1L) { fail(paste0("repeat cell missing: ", key)); next }
      for (metric in names(policy$repeat_tolerances)) {
        delta <- abs(cell$metrics[[metric]] - other[[1L]]$metrics[[metric]])
        if (!isTRUE(length(delta) == 1L && is.finite(delta) && delta <= policy$repeat_tolerances[[metric]])) {
          fail(paste0("repeat QA drift exceeds tolerance: ", key, "/", metric))
        }
      }
    }
  }
  for (name in qualification_required_reviews) {
    review <- qa$reviews[[name]]
    if (!is.list(review) || !isTRUE(review$passed) || !is.character(review$reviewer) ||
        length(review$reviewer) != 1L || !nzchar(review$reviewer) ||
        !qualification_is_sha256(review$evidence_sha256) ||
        !qualification_is_receipt(review$evidence_receipt) ||
        !identical(review$evidence_sha256, review$evidence_receipt$sha256)) {
      fail(paste0("qa review is incomplete or failed: ", name))
    }
  }
  failures <- unique(failures)
  list(passed = !length(failures), failures = failures)
}

validate_qualification_bindings <- function(qa, candidate_dir = NULL) {
  failures <- character()
  receipt_path <- function(receipt, base = NULL) {
    path <- receipt$path
    if (!is.null(base) && !grepl("^/", path)) path <- file.path(base, path)
    path
  }
  all_receipts <- c(qa$inputs, qa$candidates, qa$repeat_candidates, list(
    build_provenance = qa$build_provenance,
    repeat_build_provenance = qa$repeat_build_provenance
  ))
  for (name in names(all_receipts)) {
    receipt <- all_receipts[[name]]
    if (qualification_is_receipt(receipt)) {
      path <- receipt_path(receipt, candidate_dir)
      if (file.exists(path) && !qualification_receipt_matches(path, receipt)) {
        failures <- c(failures, paste0("receipt does not match file: ", name))
      }
    }
  }
  if (!is.null(candidate_dir)) {
    for (name in c("forward", "inverse")) {
      receipt <- qa$candidates[[name]]
      if (qualification_is_receipt(receipt) && !qualification_receipt_matches(receipt_path(receipt, candidate_dir), receipt)) {
        failures <- c(failures, paste0("candidate receipt does not match file: ", name))
      }
    }
  }
  unique(failures)
}

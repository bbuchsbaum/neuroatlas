# This narrower policy never confers approval on neuroatlas-built registrations.
evaluate_templateflow_provider <- function(calibration, policy) {
  failures <- character()
  fail <- function(message) failures <<- c(failures, message)
  if (!identical(policy$provider, "templateflow") ||
      !identical(policy$qualification_scope, "upstream_transform_application")) {
    fail("Unexpected provider qualification scope")
  }
  cells <- calibration$baselines$official_templateflow_h5$cells
  keys <- vapply(cells, function(cell) paste0(cell$direction, "_", cell$resolution, "mm"), character(1))
  if (!identical(sort(keys), sort(unlist(policy$required_cells)))) {
    return(list(passed = FALSE, failures = "Missing or duplicated provider cells"))
  }
  for (index in seq_along(cells)) {
    cell <- cells[[index]]; key <- keys[[index]]; metric <- cell$metrics
    required <- c("mi_cost", "cc_cost", "mask_dice", "label_dice_min",
      "point_error_max_mm", "roundtrip_error_max_mm", "scalar_error_max",
      "probability_min", "probability_max", "jacobian_min",
      "nonpositive_jacobians", "nonfinite_jacobians")
    finite <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
    if (!all(vapply(metric[required], finite, logical(1)))) {
      fail(paste(key, "has missing or nonfinite measurements")); next
    }
    invariants <- unlist(policy$invariants)
    if (!all(vapply(cell$invariants[invariants], isTRUE, logical(1)))) {
      fail(paste(key, "has failed application invariants"))
    }
    if (metric$point_error_max_mm > policy$point_error_max_mm ||
        metric$roundtrip_error_max_mm > policy$roundtrip_error_max_mm ||
        !finite(cell$raw$scalar_tolerance) ||
        metric$scalar_error_max > cell$raw$scalar_tolerance ||
        metric$probability_min < policy$probability_bound[[1]] ||
        metric$probability_max > policy$probability_bound[[2]] ||
        metric$jacobian_min <= 0 || metric$nonpositive_jacobians != 0 ||
        metric$nonfinite_jacobians != 0) {
      fail(paste(key, "violates a frozen numerical bound"))
    }
    improvement <- calibration$official_improves_identity[[key]]
    if (!all(vapply(improvement[unlist(policy$identity_improvement)], isTRUE, logical(1)))) {
      fail(paste(key, "does not improve the identity baseline"))
    }
  }
  list(passed = !length(failures), failures = unique(failures))
}

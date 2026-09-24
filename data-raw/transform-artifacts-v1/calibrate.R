entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- if (length(entry) == 1L) dirname(normalizePath(entry, mustWork = TRUE)) else normalizePath(file.path(getwd(), "data-raw", "transform-artifacts-v1"), mustWork = TRUE)
source(file.path(root, "scripts", "common.R"))
source(file.path(root, "scripts", "measure-qualification.R"))

run_calibration <- function(output) {
  inputs <- read_json(file.path(work_root(), "qualification-inputs.json"))
  route <- route_by_id(inputs$route_id)
  assert_release_software(route)
  for (receipt in inputs$files) {
    assert_file_receipt(measure_receipt_path(receipt), receipt)
  }
  config <- Sys.getenv("NIFLOWR_CONFIG", unset = file.path(root, "niflowr.nibi.yml"))
  settings <- niflowr::ni_config(config_file = config, auto_read = TRUE)
  lock <- settings$runtime$lockfile
  if (!grepl("^/", lock)) lock <- file.path(dirname(config), lock)
  niflowr::ni_lock_validate(lock, profiles = "ants", strict = TRUE)
  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "4")
  official_forward <- measure_receipt_path(inputs$files$official_forward)
  official_inverse <- measure_receipt_path(inputs$files$official_inverse)
  identity_file <- file.path(dirname(output), "identity-affine.tfm")
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("#Insight Transform File V1.0", "#Transform 0",
    "Transform: AffineTransform_double_3_3",
    "Parameters: 1 0 0 0 1 0 0 0 1 0 0 0", "FixedParameters: 0 0 0"), identity_file)
  neurotransform::read_linear_transform(identity_file, format = "itk",
    source = "identity-source", target = "identity-target")
  identity_cells <- unlist(lapply(c("forward", "inverse"), function(direction) lapply(c(1L, 2L), function(resolution) {
    measure_qualification_cell(direction, resolution, inputs, identity_file,
      identity_file, file.path(dirname(output), "identity-raw", paste0(direction, "-", resolution, "mm")), route, identity = TRUE)
  })), recursive = FALSE)
  cells <- unlist(lapply(c("forward", "inverse"), function(direction) lapply(c(1L, 2L), function(resolution) {
    measure_qualification_cell(direction, resolution, inputs, official_forward,
      official_inverse, file.path(dirname(output), "calibration-raw", paste0(direction, "-", resolution, "mm")), route)
  })), recursive = FALSE)
  # Measure native metric repeatability on the same frozen warped image.
  # Image/label Dice are exact counts; there is no stochastic Dice margin.
  for (index in seq_along(cells)) {
    cell <- cells[[index]]
    fixed <- if (cell$direction == "forward") "target" else "source"
    path <- function(kind) measure_receipt_path(inputs$files[[
      paste(fixed, cell$resolution, kind, sep = "_")]])
    repeated <- vapply(c(MI = 32L, CC = 4L), function(parameter) {
      metric <- if (parameter == 32L) "MI" else "CC"
      run <- niflowr::ni_run(niflowr::ni_call("ants.measure_image_similarity",
        dimension = 3, fixed_image = path("image"),
        moving_image = cell$paths$warped_image, metric = metric,
        radius_or_number_of_bins = parameter, fixed_image_mask = path("mask"),
        .engine = route$registration$engine, .profile = route$registration$profile),
        timeout = route$registration$timeout_seconds, echo = FALSE)
      getFromNamespace("ni_parse_similarity", "niflowr")(run$runtime$stdout, metric)
    }, numeric(1))
    cell$raw$repeated_similarity <- repeated
    cell$raw$similarity_noise <- abs(repeated - c(
      MI = cell$metrics$mi_cost, CC = cell$metrics$cc_cost))
    cells[[index]] <- cell
  }
  threshold_for <- function(official, identity) {
    metric <- official$metrics
    # The fixed 1e-6 floor covers the native tool's printed decimal precision;
    # larger observed repeat variation is retained explicitly in the report.
    noise <- pmax(official$raw$similarity_noise, 1e-6)
    out <- list(
      mi_cost = list(max = metric$mi_cost + noise[["MI"]]), cc_cost = list(max = metric$cc_cost + noise[["CC"]]),
      mask_dice = list(min = metric$mask_dice), label_dice_min = list(min = metric$label_dice_min),
      point_error_max_mm = list(max = 1e-3), roundtrip_error_max_mm = list(max = 0.5),
      repeat_point_error_max_mm = list(max = 0.1), scalar_error_max = list(max = official$raw$scalar_tolerance),
      probability_min = list(min = -1e-6), probability_max = list(max = 1 + 1e-6),
      jacobian_min = list(min = 0), nonpositive_jacobians = list(max = 0), nonfinite_jacobians = list(max = 0)
    )
    out
  }
  keys <- vapply(cells, function(cell) paste0(cell$direction, "_", cell$resolution, "mm"), character(1))
  identity_keys <- vapply(identity_cells, function(cell) paste0(cell$direction, "_", cell$resolution, "mm"), character(1))
  proposed <- setNames(lapply(keys, function(key) threshold_for(cells[[match(key, keys)]], identity_cells[[match(key, identity_keys)]])), keys)
  improvement <- setNames(lapply(keys, function(key) {
    reference <- cells[[match(key, keys)]]$metrics
    baseline <- identity_cells[[match(key, identity_keys)]]$metrics
    list(mi_cost = reference$mi_cost < baseline$mi_cost,
         cc_cost = reference$cc_cost < baseline$cc_cost,
         mask_dice = reference$mask_dice > baseline$mask_dice,
         label_dice_min = reference$label_dice_min > baseline$label_dice_min)
  }), keys)
  write_json(list(schema_version = 1, route_id = route$route_id,
    release_approval = FALSE, inputs = inputs$files,
    runtime_files = lapply(c(config, lock, file.path(root, "calibrate.R"),
      file.path(root, "scripts", "measure-qualification.R")), file_receipt),
    software = software_receipts(c("niflowr", "neurotransform", "RNifti", "neuroim2")),
    baselines = list(identity = list(path = identity_file, receipt = file_receipt(identity_file), cells = identity_cells), official_templateflow_h5 = list(cells = cells)),
    official_improves_identity = improvement,
    per_label_reference = setNames(lapply(cells, function(cell) cell$raw$label_dice$per_label), keys),
    proposed_thresholds = list(cells = proposed),
    note = "Candidate transforms were not read. Proposed thresholds require an independent policy review."), output)
  invisible(output)
}

if (sys.nframe() == 0L && !interactive()) {
  arguments <- commandArgs(trailingOnly = TRUE)
  output <- if (length(arguments) == 2L && arguments[[1L]] == "--output") arguments[[2L]] else read_campaign_json("RS_OUTPUTS_JSON")$calibration
  if (is.null(output) || !nzchar(output)) stop("Provide --output <calibration.json> or RS_OUTPUTS_JSON$calibration.", call. = FALSE)
  run_calibration(output)
}

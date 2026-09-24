entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))

params <- read_campaign_json("RS_PARAMS_JSON")
outputs <- read_campaign_json("RS_OUTPUTS_JSON")
required_outputs <- c("forward", "inverse", "warped_image", "provenance", "receipt")
if (!identical(params$route_id, "synthetic-testing-only") ||
    !all(required_outputs %in% names(outputs))) {
  stop("Unexpected smoke campaign contract.", call. = FALSE)
}
runtime_packages <- c("RNifti", "niflowr", "neurotransform", "neuroim2", "hdf5r")
invisible(lapply(runtime_packages, require_namespace))
if (!"ni_ants_register_to_template" %in% getNamespaceExports("niflowr")) {
  stop("Smoke requires the updated niflowr template-registration API.", call. = FALSE)
}
config <- Sys.getenv("NIFLOWR_CONFIG", unset = file.path(artifact_root(), "niflowr.nibi.yml"))
if (!file.exists(config)) stop("Missing Nibi niflowr config: ", config, call. = FALSE)
niflowr_config <- niflowr::ni_config(config_file = config, auto_read = TRUE)
lock <- niflowr_config$runtime$lockfile
if (!grepl("^/", lock)) lock <- file.path(dirname(config), lock)
if (!file.exists(lock)) stop("Missing pinned niflowr lockfile: ", lock, call. = FALSE)
niflowr::ni_lock_validate(lock, profiles = "ants", strict = TRUE)
runtime_files <- c(
  smoke = file.path(artifact_root(), "smoke.R"),
  common = file.path(artifact_root(), "scripts", "common.R"),
  config = config,
  lock = lock
)
runtime_file_receipts <- lapply(runtime_files, file_receipt)

smoke_dir <- dirname(outputs$receipt)
dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)
fixed <- file.path(smoke_dir, "synthetic-fixed.nii.gz")
moving <- file.path(smoke_dir, "synthetic-shifted.nii.gz")
fixed_mask <- file.path(smoke_dir, "synthetic-fixed-mask.nii.gz")
write_phantom <- function() {
  dimensions <- c(40L, 40L, 40L)
  grid <- arrayInd(seq_len(prod(dimensions)), dimensions)
  blob <- exp(-rowSums((grid - rep(c(20, 20, 20), each = nrow(grid)))^2) / 100)
  image <- array(blob, dimensions)
  shifted <- array(0, dimensions)
  shifted[4:40, , ] <- image[1:37, , ]
  RNifti::writeNifti(image, fixed)
  RNifti::writeNifti(shifted, moving)
  RNifti::writeNifti(array(as.numeric(image > 0.05), dimensions), fixed_mask)
}
write_phantom()

prefix <- file.path(smoke_dir, "niflowr_")
result <- niflowr::ni_ants_register_to_template(
  fixed_image = fixed,
  moving_image = moving,
  output_prefix = prefix,
  preset = "testing",
  fixed_image_mask = fixed_mask,
  random_seed = 1,
  .engine = "apptainer",
  .profile = "ants",
  timeout = 1800,
  echo = TRUE
)
copy_output <- function(from, to) {
  if (!file.exists(from) || !file.copy(from, to, copy.date = TRUE)) {
    stop("Could not publish smoke output ", from, " to ", to, call. = FALSE)
  }
}
copy_output(result$outputs$composite_transform, outputs$forward)
copy_output(result$outputs$inverse_composite_transform, outputs$inverse)
copy_output(result$outputs$warped_image, outputs$warped_image)
write_json(
  list(
    schema_version = 1,
    production_evidence = FALSE,
    route_id = params$route_id,
    preset = "testing",
    inputs = list(fixed = fixed, moving = moving, fixed_mask = fixed_mask),
    outputs = lapply(outputs[c("forward", "inverse", "warped_image")], function(path) {
      list(path = path, bytes = unname(file.info(path)$size), sha256 = sha256_file(path))
    }),
    software = software_receipts(runtime_packages),
    runtime_files = runtime_file_receipts,
    campaign = Sys.getenv("RS_CAMPAIGN", unset = NA_character_),
    run_id = Sys.getenv("RS_RUN_ID", unset = NA_character_),
    attempt_id = Sys.getenv("RS_ATTEMPT_ID", unset = NA_character_)
  ),
  outputs$provenance
)
write_json(
  list(
    schema_version = 1,
    production_evidence = FALSE,
    route_id = params$route_id,
    campaign = Sys.getenv("RS_CAMPAIGN", unset = NA_character_),
    run_id = Sys.getenv("RS_RUN_ID", unset = NA_character_),
    attempt_id = Sys.getenv("RS_ATTEMPT_ID", unset = NA_character_),
    purpose = "Nibi campaign plus niflowr testing-preset ANTs canary"
  ),
  outputs$receipt
)

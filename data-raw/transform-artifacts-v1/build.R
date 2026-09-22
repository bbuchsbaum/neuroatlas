entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))

runtime_packages <- c("RNifti", "niflowr", "neurotransform", "neuroim2", "hdf5r")
invisible(lapply(runtime_packages, require_namespace))
route_id <- route_id_from_args_or_campaign()
route <- route_by_id(route_id)
outputs <- read_campaign_json("RS_OUTPUTS_JSON", required = FALSE)
if (length(outputs) == 0L) {
  stop("build.R must be run through a campaign with declared outputs.", call. = FALSE)
}
required_outputs <- c("forward", "inverse", "warped_image", "provenance")
if (!all(required_outputs %in% names(outputs))) {
  stop("Campaign outputs must contain: ", paste(required_outputs, collapse = ", "), call. = FALSE)
}
if (any(vapply(outputs[required_outputs], file.exists, logical(1)))) {
  stop("Refusing to overwrite a declared campaign output.", call. = FALSE)
}

config <- Sys.getenv("NIFLOWR_CONFIG", unset = file.path(artifact_root(), "niflowr.nibi.yml"))
if (!file.exists(config)) {
  stop("Missing Nibi-specific niflowr config: ", config,
       ". Start from niflowr.nibi.yml.template after recording rslurm info.", call. = FALSE)
}
niflowr_config <- niflowr::ni_config(config_file = config, auto_read = TRUE)
if (!"ni_ants_register_to_template" %in% getNamespaceExports("niflowr")) {
  stop(
    "Installed niflowr lacks ni_ants_register_to_template(). Install ",
    route$registration$niflowr_development_ref,
    " before building.",
    call. = FALSE
  )
}
lock <- niflowr_config$runtime$lockfile
if (!grepl("^/", lock)) lock <- file.path(dirname(config), lock)
if (!file.exists(lock)) {
  stop("Missing pinned niflowr lockfile: ", lock, call. = FALSE)
}
niflowr::ni_lock_validate(lock, profiles = "ants", strict = TRUE)
runtime_files <- c(
  build = file.path(artifact_root(), "build.R"),
  common = file.path(artifact_root(), "scripts", "common.R"),
  config = config,
  lock = lock
)
runtime_file_receipts <- lapply(runtime_files, file_receipt)

inputs <- route_input_specs(route)
input_root <- work_root(create = FALSE)
paths <- lapply(inputs, materialized_input_path, root = input_root)
invisible(mapply(assert_file_receipt, paths, inputs, SIMPLIFY = FALSE))

out_dir <- dirname(outputs$forward)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
prefix <- file.path(out_dir, "niflowr_")
old_threads <- Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS", unset = NA_character_)
on.exit({
  if (is.na(old_threads)) Sys.unsetenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS") else {
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = old_threads)
  }
}, add = TRUE)
Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = as.character(route$registration$threads))

result <- niflowr::ni_ants_register_to_template(
  fixed_image = paths$target_image,
  moving_image = paths$source_image,
  output_prefix = prefix,
  preset = route$registration$preset,
  fixed_image_mask = paths$target_mask,
  random_seed = route$registration$random_seed,
  .engine = route$registration$engine,
  .profile = route$registration$profile,
  timeout = route$registration$timeout_seconds,
  echo = TRUE
)

copy_output <- function(from, to) {
  if (!file.exists(from) || !file.copy(from, to, copy.date = TRUE)) {
    stop("Could not publish niflowr output ", from, " to ", to, call. = FALSE)
  }
  invisible(to)
}
copy_output(result$outputs$composite_transform, outputs$forward)
copy_output(result$outputs$inverse_composite_transform, outputs$inverse)
copy_output(result$outputs$warped_image, outputs$warped_image)
write_json(
  list(
    schema_version = 1,
    route_id = route_id,
    attempt_id = Sys.getenv("RS_ATTEMPT_ID", unset = NA_character_),
    registration = route$registration,
    niflowr = list(
      development_ref = route$registration$niflowr_development_ref,
      last_observed_commit = route$registration$niflowr_last_observed_commit,
      observed_package = package_receipt("niflowr")
    ),
    software = software_receipts(runtime_packages),
    runtime_files = runtime_file_receipts,
    inputs = lapply(paths, function(path) list(path = path, bytes = unname(file.info(path)$size), sha256 = sha256_file(path))),
    outputs = lapply(outputs[c("forward", "inverse", "warped_image")], function(path) list(path = path, bytes = unname(file.info(path)$size), sha256 = sha256_file(path))),
    niflowr_result = result
  ),
  outputs$provenance
)

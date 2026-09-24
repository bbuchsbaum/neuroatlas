entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
root <- dirname(normalizePath(entry_file, mustWork = TRUE))
source(file.path(root, "scripts", "common.R"))
source(file.path(root, "scripts", "measure-qualification.R"))
source(file.path(root, "scripts", "qualification-gates.R"))
source(file.path(root, "scripts", "visual-qa.R"))

route <- route_by_id(route_id_from_args_or_campaign())
policy_path <- file.path(root, "qualification-policy.json")
policy <- read_json(policy_path)
policy_errors <- qualification_policy_errors(policy)
if (length(policy_errors)) stop(paste(policy_errors, collapse = "; "), call. = FALSE)
calibration_path <- Sys.getenv("NEUROATLAS_TRANSFORM_CALIBRATION", unset = "")
if (!file.exists(calibration_path) ||
    !identical(sha256_file(calibration_path), policy$calibration_sha256)) {
  stop("The frozen calibration receipt is missing or changed.", call. = FALSE)
}
# No candidate bytes or quality values are read before the policy checks above.
assert_release_software(route)
config <- file.path(root, "niflowr.nibi.yml")
settings <- niflowr::ni_config(config_file = config, auto_read = TRUE,
  config = list(env = list(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "4")))
lock <- settings$runtime$lockfile
if (!grepl("^/", lock)) lock <- file.path(root, lock)
niflowr::ni_lock_validate(lock, profiles = "ants", strict = TRUE)
Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "4")
inputs <- read_json(file.path(work_root(), "qualification-inputs.json"))
input_paths <- lapply(inputs$files, measure_receipt_path)
invisible(mapply(assert_file_receipt, input_paths, inputs$files, SIMPLIFY = FALSE))
outputs <- read_campaign_json("RS_OUTPUTS_JSON")
required <- c("qa_json", "qa_report", "qa_visual_manifest", "qa_visual_report")
if (!all(required %in% names(outputs))) stop("Qualification campaign lacks outputs.")
if (any(vapply(outputs[required], file.exists, logical(1)))) stop("Refusing to overwrite qualification outputs.")
candidate_dir <- Sys.getenv("NEUROATLAS_TRANSFORM_CANDIDATE_DIR", unset = "")
repeat_dir <- Sys.getenv("NEUROATLAS_TRANSFORM_REPEAT_DIR", unset = "")
if (!dir.exists(candidate_dir) || !dir.exists(repeat_dir)) stop("Provide candidate and repeat build directories.")
candidates <- lapply(route$outputs[c("forward", "inverse")], function(name) file.path(candidate_dir, name))
repeated <- lapply(route$outputs[c("forward", "inverse")], function(name) file.path(repeat_dir, name))
provenance <- file.path(candidate_dir, "build-provenance.json")
repeat_provenance <- file.path(repeat_dir, "build-provenance.json")
for (paths in list(candidates, repeated)) invisible(lapply(paths, file_receipt))
qa_dir <- dirname(outputs$qa_json)
landmark_manifest_path <- file.path(root, "landmarks-v1", "manifest.json")
assert_file_receipt(landmark_manifest_path, policy$landmarks)
landmark_manifest <- read_json(landmark_manifest_path)
cells <- list(); repeat_cells <- list(); visual_files <- list()
for (direction in c("forward", "inverse")) for (resolution in c(1L, 2L)) {
  key <- paste0(direction, "_", resolution, "mm")
  fixed_side <- if (direction == "forward") "target" else "source"
  landmark_spec <- landmark_manifest$cells[[paste(fixed_side, resolution, sep = "_")]]
  landmark_path <- file.path(root, "landmarks-v1", landmark_spec$file)
  assert_file_receipt(landmark_path, landmark_spec$receipt)
  landmarks <- utils::read.csv(landmark_path)
  cell <- measure_qualification_cell(direction, resolution, inputs,
    candidates$forward, candidates$inverse, file.path(qa_dir, "raw", key), route,
    repeat_forward = repeated$forward, repeat_inverse = repeated$inverse,
    landmarks = landmarks)
  repeat_cells[[key]] <- measure_qualification_cell(direction, resolution, inputs,
    repeated$forward, repeated$inverse, file.path(qa_dir, "repeat-raw", key), route,
    repeat_forward = candidates$forward, repeat_inverse = candidates$inverse,
    landmarks = landmarks)
  cells[[key]] <- cell
  moving <- if (direction == "forward") "source" else "target"
  fixed <- if (direction == "forward") "target" else "source"
  get_path <- function(side, kind, res = resolution) input_paths[[paste(side, res, kind, sep = "_")]]
  visual <- render_visual_qa(get_path(moving, "image", 1L), get_path(fixed, "image"),
    cell$paths$warped_image, get_path(fixed, "mask"), cell$paths$warped_mask,
    get_path(fixed, "qa_labels"), cell$paths$nearest_labels, cell$paths$jacobian,
    file.path(dirname(outputs$qa_visual_manifest), key),
    source_space = if (direction == "forward") route$source_space else route$target_space,
    target_space = if (direction == "forward") route$target_space else route$source_space,
    label_interpolation = "NearestNeighbor")
  for (name in names(visual$files)) {
    receipt <- visual$files[[name]]
    receipt$path <- file.path(key, receipt$path)
    visual_files[[paste(key, name, sep = "/")]] <- receipt
  }
}
write_json(list(schema_version = 1, qualitative_only = TRUE,
  files = visual_files), outputs$qa_visual_manifest)
writeLines(c("<!doctype html><html><body><h1>Four-cell visual evidence</h1>",
  vapply(names(cells), function(key) paste0('<p><a href="', key, '/index.html">', key, '</a></p>'), character(1)),
  "</body></html>"), outputs$qa_visual_report)
review_path <- Sys.getenv("NEUROATLAS_TRANSFORM_REVIEWS", unset = "")
reviews <- if (file.exists(review_path)) read_json(review_path) else list()
qa <- list(schema_version = 2, route_id = route$route_id,
  policy_sha256 = sha256_file(policy_path), calibration_sha256 = sha256_file(calibration_path),
  inputs = lapply(input_paths, file_receipt), candidates = lapply(candidates, file_receipt),
  repeat_candidates = lapply(repeated, file_receipt),
  build_provenance = file_receipt(provenance), repeat_build_provenance = file_receipt(repeat_provenance),
  software = software_receipts(c("niflowr", "neurotransform", "neuroim2", "RNifti", "hdf5r")),
  execution_environment = as.list(settings$env),
  runtime_files = lapply(c(config, lock, file.path(root, "qualify.R"),
    file.path(root, "scripts", "measure-qualification.R")), file_receipt),
  cells = unname(cells), repeat_cells = unname(repeat_cells),
  landmarks = file_receipt(landmark_manifest_path), reviews = reviews, visual_qa = file_receipt(outputs$qa_visual_manifest))
verdict <- evaluate_qualification(qa, policy)
qa$release_eligible <- verdict$passed
qa$gate_failures <- verdict$failures
writeLines(c("<!doctype html><html><body><h1>Transform qualification</h1>",
  paste0("<p>Release eligible: ", qa$release_eligible, "</p>"),
  "<p>All numerical evidence and retained failures are in qa.json.</p>",
  '<p><a href="visual/index.html">Visual evidence for both directions and grids</a></p>',
  "</body></html>"), outputs$qa_report)
qa$report <- file_receipt(outputs$qa_report)
write_json(qa, outputs$qa_json)
message("Release eligible: ", verdict$passed)
if (!verdict$passed) message(paste(verdict$failures, collapse = "\n"))

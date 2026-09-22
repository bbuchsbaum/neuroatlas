entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))
source(file.path(artifact_root(), "scripts", "visual-qa.R"))

runtime_packages <- c("RNifti", "niflowr", "neurotransform", "neuroim2", "hdf5r")
invisible(lapply(runtime_packages, require_namespace))
route_id <- route_id_from_args_or_campaign()
route <- route_by_id(route_id)
policy <- read_json(file.path(artifact_root(), "qualification-policy.json"))
outputs <- read_campaign_json("RS_OUTPUTS_JSON", required = FALSE)
if (!all(c("qa_json", "qa_report", "qa_visual_manifest", "qa_visual_report") %in% names(outputs))) {
  stop("qualification campaign must expose JSON, HTML, and visual-QA outputs.", call. = FALSE)
}
route_root <- dirname(dirname(outputs$qa_json))
candidates <- list(
  forward = file.path(route_root, "build", route$outputs$forward),
  inverse = file.path(route_root, "build", route$outputs$inverse)
)

# This is intentionally before any candidate measurement. A draft policy must
# never produce an apparently passing report whose thresholds were chosen after
# seeing its values.
if (!isTRUE(policy$release_approval$approved) || is.null(policy$thresholds)) {
  stop("Qualification policy is not approved with frozen numeric thresholds; refusing candidate evaluation.", call. = FALSE)
}

inputs <- route_input_specs(route)
input_root <- work_root(create = FALSE)
paths <- lapply(inputs, materialized_input_path, root = input_root)
invisible(mapply(assert_file_receipt, paths, inputs, SIMPLIFY = FALSE))
for (name in c("forward", "inverse")) {
  if (!file.exists(candidates[[name]]) || file.info(candidates[[name]])$size < 1L) {
    stop("Missing transform candidate: ", candidates[[name]], call. = FALSE)
  }
}

# ANTs builds the candidate; this independent parser catches malformed or
# convention-incompatible H5 before a transform can be released.
forward_transform <- neurotransform::read_transform(candidates$forward, type = "ants_h5")
inverse_transform <- neurotransform::read_transform(candidates$inverse, type = "ants_h5")
if (is.null(forward_transform) || is.null(inverse_transform)) {
  stop("neurotransform did not return both ANTs H5 transforms.", call. = FALSE)
}

qa <- niflowr::ni_ants_registration_qa(
  fixed_image = paths$target_image,
  moving_image = paths$source_image,
  transform = candidates$forward,
  fixed_mask = paths$target_mask,
  moving_mask = paths$source_mask,
  fixed_labels = paths$target_qa_labels,
  moving_labels = paths$source_qa_labels,
  .engine = route$registration$engine,
  .profile = route$registration$profile,
  timeout = route$registration$timeout_seconds,
  out_dir = dirname(outputs$qa_json),
  echo = TRUE
)
visual <- render_visual_qa(
  source_image = paths$source_image,
  target_image = paths$target_image,
  warped_image = qa$files$warped_image,
  target_mask = paths$target_mask,
  warped_mask = qa$files$warped_mask,
  target_labels = paths$target_qa_labels,
  warped_labels = qa$files$warped_labels,
  jacobian = qa$files$jacobian,
  output_dir = dirname(outputs$qa_visual_report)
)
if (!identical(normalizePath(file.path(dirname(outputs$qa_visual_report), "visual-qa.json")),
               normalizePath(outputs$qa_visual_manifest, mustWork = FALSE))) {
  stop("Campaign visual manifest must be visual-qa.json beside the visual report.", call. = FALSE)
}
write_json(
  list(
    schema_version = 1,
    route_id = route_id,
    policy = policy,
    software = software_receipts(runtime_packages),
    qa = qa,
    visual_qa = visual,
    release_eligible = FALSE
  ),
  outputs$qa_json
)
writeLines(
  c("<html><body><h1>Transform qualification</h1>",
    "<p>Machine-readable evidence: ", basename(outputs$qa_json), "</p>",
    "<p>Visual review: <a href=\"visual/index.html\">before/after QA panels</a>.</p>",
    "<p>Release eligibility remains false until all frozen gates are evaluated.</p>",
    "</body></html>"),
  outputs$qa_report
)

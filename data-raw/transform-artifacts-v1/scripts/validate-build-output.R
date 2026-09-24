arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 4L) {
  stop("Usage: validate-build-output.R <forward.h5> <inverse.h5> <warped.nii.gz> <provenance.json>", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("neurotransform", quietly = TRUE)) {
  stop("jsonlite and neurotransform are required for build validation.", call. = FALSE)
}
for (path in arguments) {
  if (!file.exists(path) || file.info(path)$size < 1L) stop("Missing or empty output: ", path, call. = FALSE)
}
neurotransform::read_transform(arguments[[1L]], type = "ants_h5")
neurotransform::read_transform(arguments[[2L]], type = "ants_h5")
provenance <- jsonlite::read_json(arguments[[4L]], simplifyVector = FALSE)
if (is.null(provenance$route_id) || is.null(provenance$outputs)) {
  stop("Build provenance lacks route_id or output receipts.", call. = FALSE)
}

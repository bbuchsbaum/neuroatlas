arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 5L) stop("Usage: validate-smoke-output.R <forward.h5> <inverse.h5> <warped.nii.gz> <provenance.json> <receipt.json>", call. = FALSE)
if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("neurotransform", quietly = TRUE)) {
  stop("jsonlite and neurotransform are required for smoke validation.", call. = FALSE)
}
for (path in arguments) {
  if (!file.exists(path) || file.info(path)$size < 1L) stop("Missing or empty smoke output: ", path, call. = FALSE)
}
neurotransform::read_transform(arguments[[1L]], type = "ants_h5")
neurotransform::read_transform(arguments[[2L]], type = "ants_h5")
provenance <- jsonlite::read_json(arguments[[4L]], simplifyVector = FALSE)
receipt <- jsonlite::read_json(arguments[[5L]], simplifyVector = FALSE)
if (!identical(receipt$production_evidence, FALSE) || !identical(receipt$route_id, "synthetic-testing-only")) {
  stop("Smoke receipt is not explicitly non-production evidence.", call. = FALSE)
}
if (!identical(provenance$preset, "testing") || !identical(provenance$production_evidence, FALSE)) {
  stop("Smoke provenance does not identify the non-production testing preset.", call. = FALSE)
}

# Manual integration gate: real public assets, an empty cache, both directions.
# Run with the candidate neuroatlas package installed or devtools::load_all().
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop("Usage: verify-live-release.R <work-root> <empty-cache> <report.json> <qualification-dir>")
}
library(neuroatlas)
entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_root <- if (length(entry) == 1L) dirname(normalizePath(entry)) else {
  file.path(getwd(), "data-raw", "transform-artifacts-v1", "scripts")
}
source(file.path(script_root, "live-release-checks.R"))
work <- normalizePath(args[[1L]], mustWork = TRUE)
cache <- args[[2L]]
report <- args[[3L]]
qualification_dir <- normalizePath(args[[4L]], mustWork = TRUE)
qa_path <- file.path(qualification_dir, "qa.json")
qa <- jsonlite::read_json(qa_path)
manifest_path <- tempfile("published-transform-manifest-", fileext = ".json")
manifest_url <- paste0("https://github.com/bbuchsbaum/neuroatlas/releases/",
  "download/transform-artifacts-v1/transform-artifacts-v1.json")
utils::download.file(manifest_url, manifest_path, mode = "wb", quiet = TRUE)
manifest <- jsonlite::read_json(manifest_path)
live_assert_receipt(qa_path, manifest$qa)
stopifnot(isTRUE(qa$release_eligible),
  identical(manifest$policy$sha256, qa$policy_sha256))
if (file.exists(report)) stop("Refusing to replace integration evidence.")
if (dir.exists(cache) && length(list.files(cache, all.files = TRUE, no.. = TRUE))) {
  stop("Public integration requires an empty cache.")
}
inventory <- jsonlite::read_json(file.path(work, "qualification-inputs.json"))
input_path <- function(side, resolution, kind) {
  key <- paste(side, resolution, kind, sep = "_")
  item <- inventory$files[[key]]
  if (!live_receipts_equal(item, qa$inputs[[key]])) {
    stop("Input inventory differs from qualified input: ", key)
  }
  path <- file.path(work, "inputs", item$relpath)
  stopifnot(file.exists(path), file.info(path)$size == item$bytes,
    identical(digest::digest(file = path, algo = "sha256"), item$sha256))
  path
}
spaces <- c(source = "MNI152NLin6Asym", target = "MNI152NLin2009cAsym")
evidence <- list()
for (direction in c("forward", "inverse")) {
  moving <- if (direction == "forward") "source" else "target"
  fixed <- if (direction == "forward") "target" else "source"
  transform <- get_template_transform(spaces[[moving]], spaces[[fixed]],
    provider = "neuroatlas", cache_dir = cache)
  asset <- manifest$assets[[direction]]
  stopifnot(transform$plan$n_steps == 1L,
    identical(asset$from_space, spaces[[moving]]),
    identical(asset$to_space, spaces[[fixed]]),
    live_receipts_equal(list(bytes = asset$size_bytes, sha256 = asset$sha256),
                        qa$candidates[[direction]]))
  live_assert_receipt(transform$files[[1L]], qa$candidates[[direction]])
  source <- neuroim2::read_vol(input_path(moving, 1, "image"))
  labels <- neuroim2::read_vol(input_path(moving, 1, "qa_labels"))
  target <- neuroim2::read_vol(input_path(fixed, 2, "image"))
  source_values <- as.array(source)
  intensity_range <- range(source_values[is.finite(source_values)])
  normalized <- (source_values - intensity_range[[1L]]) / diff(intensity_range)
  normalized[!is.finite(normalized)] <- 0
  source <- neuroim2::NeuroVol(normalized, neuroim2::space(source))
  scalar <- apply_template_transform(source, transform, target,
                                     data_type = "continuous")
  labelled <- apply_template_transform(labels, transform, target,
                                       data_type = "label")
  stopifnot(identical(dim(scalar), dim(target)),
    identical(dim(labelled), dim(target)),
    identical(as.numeric(neuroim2::trans(scalar)),
              as.numeric(neuroim2::trans(target))),
    identical(as.numeric(neuroim2::trans(labelled)),
              as.numeric(neuroim2::trans(target))))
  scalar_values <- as.array(scalar)
  label_values <- as.array(labelled)
  source_ids <- sort(unique(as.vector(as.array(labels))))
  stopifnot(all(is.finite(scalar_values)), all(is.finite(label_values)),
    all(label_values == round(label_values)),
    all(label_values %in% c(0, source_ids)))
  # Compare the public apply path with separately executed native ANTs outputs.
  key <- paste0(direction, "_2mm")
  reference_dir <- file.path(qualification_dir, "raw", key)
  reference_scalar <- file.path(reference_dir, "scalar-linear.nii.gz")
  reference_labels <- file.path(reference_dir, "labels-nearest.nii.gz")
  allowed_path <- file.path(reference_dir,
    "label-oracle-allowed-differences.nii.gz")
  cell <- Filter(function(x) identical(x$direction, direction) &&
    identical(as.numeric(x$resolution), 2), qa$cells)[[1L]]
  references <- list(scalar = reference_scalar, labels = reference_labels,
                     allowed_label_differences = allowed_path)
  for (name in names(references)) {
    live_assert_receipt(references[[name]], cell$reference_outputs[[name]])
    live_assert_grid(references[[name]], input_path(fixed, 2, "image"))
  }
  fixed_mask <- as.array(RNifti::readNifti(input_path(fixed, 2, "mask"))) > 0
  native_scalar <- as.array(RNifti::readNifti(reference_scalar))
  native_labels <- as.array(RNifti::readNifti(reference_labels))
  allowed <- as.array(RNifti::readNifti(allowed_path))
  scalar_error <- max(abs(scalar_values[fixed_mask] - native_scalar[fixed_mask]))
  label_disagreements <- live_label_disagreements(label_values, native_labels,
                                                 allowed)
  stopifnot(is.finite(scalar_error), scalar_error <= cell$raw$scalar_tolerance)
  # Reopen only from the verified cache and prove identical application.
  offline <- get_template_transform(spaces[[moving]], spaces[[fixed]],
    provider = "neuroatlas", cache_dir = cache, offline = TRUE)
  again <- apply_template_transform(labels, offline, target, data_type = "label")
  stopifnot(identical(as.array(again), label_values))
  receipt <- attr(labelled, "neuroatlas_transform")
  stopifnot(identical(receipt$from_space, spaces[[moving]]),
    identical(receipt$to_space, spaces[[fixed]]),
    identical(receipt$interpolation, "nearest"),
    identical(receipt$renormalized, FALSE))
  evidence[[direction]] <- list(
    from_space = spaces[[moving]], to_space = spaces[[fixed]],
    target_dimensions = dim(target), target_affine = neuroim2::trans(target),
    artifacts = transform$plan$steps,
    scalar_range = range(scalar_values),
    native_scalar_error_in_mask = scalar_error,
    native_label_disagreements = label_disagreements,
    native_reference_sha256 = list(
      scalar = digest::digest(file = reference_scalar, algo = "sha256"),
      labels = digest::digest(file = reference_labels, algo = "sha256")),
    label_ids = sort(unique(as.vector(label_values))),
    lost_label_ids = setdiff(source_ids[source_ids != 0], unique(label_values)),
    exact_target_geometry = TRUE, finite_outputs = TRUE,
    integer_labels = TRUE, no_new_labels = TRUE, offline_identical = TRUE)
  rm(transform, offline, source, labels, target, scalar, labelled, again,
     scalar_values, label_values, source_values, normalized, native_scalar,
     native_labels, fixed_mask, allowed)
  gc()
}
jsonlite::write_json(list(schema_version = 1, passed = TRUE,
  measured_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  neuroatlas = as.character(packageVersion("neuroatlas")),
  neurotransform = as.character(packageVersion("neurotransform")),
  neurotransform_commit = packageDescription("neurotransform")$RemoteSha,
  published_manifest_sha256 = digest::digest(file = manifest_path, algo = "sha256"),
  qualification_sha256 = digest::digest(file = qa_path, algo = "sha256"),
  cold_cache = TRUE, anonymous_https = TRUE, evidence = evidence),
  report, auto_unbox = TRUE, pretty = TRUE, digits = NA)

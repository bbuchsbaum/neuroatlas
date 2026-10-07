# Full-density adapter checks against direct public surface application.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
root <- args[[1]]
python <- args[[2]]
stopifnot(file.exists(python))
cache <- normalizePath(args[[3]], mustWork = TRUE)
dir.create(root, recursive = TRUE)
devtools::load_all()
sha <- function(path) digest::digest(file = path, algo = "sha256")
sources <- c("DESCRIPTION", "NAMESPACE", "R/cifti.R",
  "R/template_transform.R", "R/surface_transform.R", "R/surface_assets.R",
  "data-raw/cifti-v1/contract-v1.json", "data-raw/cifti-v1/qualify-adapter.R",
  "data-raw/cifti-v1/adapter-fixtures.py")
binding <- setNames(lapply(sources, sha), sources)
geometries <- list()
for (template in c("fsaverage", "fsLR")) {
  geometries[[template]] <- setNames(lapply(c("L", "R"), function(hemi) {
    get_surface_geometry(template,
      if (template == "fsaverage") "164k" else "32k", hemi,
      cache_dir = cache, offline = TRUE)
  }), c("L", "R"))
}
write_double <- function(values, path) {
  connection <- file(path, "wb")
  on.exit(close(connection))
  writeBin(as.double(values), connection, size = 8L, endian = "little")
}
reports <- list()
for (direction in c("down", "up")) {
  directory <- file.path(root, direction)
  dir.create(directory)
  from <- geometries[[if (direction == "down") "fsaverage" else "fsLR"]]
  to <- geometries[[if (direction == "down") "fsLR" else "fsaverage"]]
  operators <- setNames(lapply(c("L", "R"), function(hemi) {
    operator <- get_template_transform(from[[hemi]], to[[hemi]],
      cache_dir = cache)
    stopifnot(identical(operator,
      get_template_transform(from[[hemi]], to[[hemi]],
        cache_dir = cache, offline = TRUE)))
    operator
  }), c("L", "R"))
  for (side in c("source", "target")) {
    geometry <- if (side == "source") from else to
    info <- lapply(geometry, function(g) {
      list(indices = which(g$cortex) - 1L, n_vertices = g$domain$n_vertices)
    })
    jsonlite::write_json(info, file.path(directory, paste0(side, ".json")),
      auto_unbox = TRUE)
  }
  fields <- list(scalar = list(), label = list())
  for (hemi in c("L", "R")) {
    g <- from[[hemi]]
    unit <- g$sphere / sqrt(rowSums(g$sphere^2))
    fields$scalar[[hemi]] <- cbind(
      unit[, 1] + if (hemi == "L") 10 else -10,
      sin(7 * unit[, 2]) * cos(5 * unit[, 3]))
    fields$label[[hemi]] <- cbind(ifelse(unit[, 1] > 0, 2, -3),
      ifelse(unit[, 2] > 0, 7, -3))
    # An internal boundary exercises both missingness and explicit label fill.
    absent <- which(g$cortex)[seq_len(100L)]
    fields$scalar[[hemi]][absent, ] <- NA_real_
    fields$label[[hemi]][absent, ] <- NA_real_
  }
  for (kind in names(fields)) {
    take <- function(hemi) fields[[kind]][[hemi]][from[[hemi]]$cortex, , drop = FALSE]
    volume <- if (kind == "scalar") matrix(c(101, -202, 303, -404), 2L) else
      matrix(c(2, -3, 7, 0), 2L)
    values <- rbind(take("R"), volume, take("L"))
    if (kind == "label") values[is.na(values)] <- 0
    write_double(values, file.path(directory, paste0(kind, "-source.bin")))
  }
  stopifnot(system2(python, c("data-raw/cifti-v1/adapter-fixtures.py",
    "generate", shQuote(directory))) == 0)
  for (kind in c("scalar", "label")) for (axis in 0:1) {
    source <- read_cifti(file.path(directory, paste0("source-", kind, "-", axis, ".nii")))
    target <- read_cifti(file.path(directory, paste0("target-", kind, "-", axis, ".nii")))
    # Declare unavailable label rows explicitly, rather than trusting zero.
    if (kind == "label") {
      available <- matrix(TRUE, nrow(source$values), 2L)
      for (model in source$brain_models) {
        hemi <- neuroatlas:::.cifti_hemisphere(model)
        if (is.null(hemi)) next
        rows <- model$offset + seq_len(model$count)
        available[rows, ] <- !is.na(fields$label[[hemi]][model$indices + 1L, ])
      }
      source$available <- available
      source$xml <- neuroatlas:::.cifti_availability_xml(source$xml, available)
      source <- neuroatlas:::.cifti_finish(source)
    }
    adapter <- get_template_transform(source, target, cortex = operators,
      volume_space = "MNI152NLin6Asym")
    result <- apply_template_transform(source, adapter,
      missing_labels = if (kind == "label") 0 else NULL)
    expected <- matrix(NA_real_, nrow(target$values), 2L)
    available <- matrix(FALSE, nrow(target$values), 2L)
    for (model in target$brain_models) {
      rows <- model$offset + seq_len(model$count)
      hemi <- neuroatlas:::.cifti_hemisphere(model)
      if (is.null(hemi)) {
        original <- Filter(function(m) m$type == "CIFTI_MODEL_TYPE_VOXELS",
          source$brain_models)[[1]]
        order <- match(model$indices[, 1], original$indices[, 1])
        expected[rows, ] <- source$values[original$offset + order, ]
        available[rows, ] <- TRUE
        next
      }
      for (map in 1:2) {
        direct <- apply_template_transform(surface_data(fields[[kind]][[hemi]][, map],
          from[[hemi]]$domain, if (kind == "label") "label" else "continuous",
          if (kind == "label") source$label_tables[[map]] else NULL), operators[[hemi]])
        values <- direct$values[model$indices + 1L]
        available[rows, map] <- !is.na(values)
        if (kind == "label") values[is.na(values)] <- 0
        expected[rows, map] <- values
      }
    }
    stopifnot(identical(result$values, expected),
      identical(result$available, available), identical(result$volume, source$volume),
      identical(result$label_tables, source$label_tables))
    file <- file.path(directory, paste0("result-", kind, "-", axis, ".nii"))
    write_cifti(result, file)
    roundtrip <- read_cifti(file)
    stopifnot(identical(roundtrip$values, expected),
      identical(roundtrip$available, available))
    write_double(expected, file.path(directory, paste0("expected-", kind, "-", axis, ".bin")))
    reports[[length(reports) + 1L]] <- list(direction = direction, kind = kind,
      brain_axis = axis, rows = nrow(expected), maps = 2L,
      max_abs_error = 0, availability_mismatches = 0,
      operators = lapply(operators, function(op) op$specification),
      output_sha256 = sha(file))
    cat(direction, kind, axis, "PASS", nrow(expected), "brainordinates\n")
  }
  stopifnot(system2(python, c("data-raw/cifti-v1/adapter-fixtures.py",
    "verify", shQuote(directory))) == 0)
}
stopifnot(identical(binding, setNames(lapply(sources, sha), sources)))
jsonlite::write_json(list(status = "PASS", source_bindings = binding,
  cases = reports, engine = jsonlite::read_json(Sys.getenv("NEUROATLAS_ENGINE_BINDING"))),
  file.path(root, "adapter-consumer-receipt.json"), auto_unbox = TRUE, pretty = TRUE,
  null = "null", digits = NA)

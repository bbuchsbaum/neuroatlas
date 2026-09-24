entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))
require_namespace("templateflow")
root <- work_root(create = TRUE)
route <- routes_definition()$routes[[1L]]
destination <- file.path(root, "qualification-inputs.json")
if (file.exists(destination)) {
  frozen <- read_json(destination)
  for (receipt in frozen$files) {
    assert_file_receipt(file.path(root, "inputs", receipt$relpath), receipt)
  }
  message("Existing qualification inputs verified; nothing overwritten.")
  quit(status = 0L)
}
files <- list()
for (side in c("source", "target")) {
  for (resolution in c(1L, 2L)) {
    for (kind in c("image", "mask", "qa_labels")) {
      query <- route[[side]][[kind]]$query
      query$resolution <- resolution
      template <- route[[side]]$template
      path <- do.call(templateflow::tf_get,
                      c(list(template = template, read = FALSE), query))
      stopifnot(is.character(path), length(path) == 1L, file.exists(path))
      relpath <- file.path(paste0("tpl-", template), basename(path))
      receipt <- file_receipt(path)
      if (resolution == 1L) assert_file_receipt(path, route[[side]][[kind]])
      spec <- c(list(template = template, query = query, relpath = relpath),
                receipt[c("bytes", "sha256")])
      materialize_input(spec, root)
      files[[paste(side, resolution, kind, sep = "_")]] <- spec
    }
  }
}
for (direction in c("forward", "inverse")) {
  from <- if (direction == "forward") route$source_space else route$target_space
  to <- if (direction == "forward") route$target_space else route$source_space
  query <- list(from = from, mode = "image", suffix = "xfm", extension = ".h5")
  path <- do.call(templateflow::tf_get,
                  c(list(template = to, read = FALSE), query))
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  receipt <- file_receipt(path)
  spec <- c(list(template = to, query = query,
                 relpath = file.path(paste0("tpl-", to), basename(path))),
            receipt[c("bytes", "sha256")])
  materialize_input(spec, root)
  files[[paste0("official_", direction)]] <- spec
}
write_json(list(schema_version = 1, route_id = route$route_id,
                role = "independent_baseline_inputs", files = files), destination)
message("Pinned ", length(files), " qualification inputs: ", destination)

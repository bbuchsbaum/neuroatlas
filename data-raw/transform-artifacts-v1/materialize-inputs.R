entry_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(entry_file, mustWork = TRUE)), "scripts", "common.R"))

routes <- routes_definition()
if (!identical(routes$status, "inputs_frozen_not_built")) {
  stop("Unexpected route-definition status: ", routes$status, call. = FALSE)
}

root <- work_root(create = TRUE)
receipts <- list()
for (route in routes$routes) {
  inputs <- route_input_specs(route)
  receipts[[route$route_id]] <- lapply(inputs, function(spec) {
    path <- materialize_input(spec, root)
    list(path = path, bytes = unname(file.info(path)$size), sha256 = sha256_file(path))
  })
}
write_json(
  list(
    schema_version = 1,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    routes = receipts
  ),
  file.path(root, "input-receipts.json")
)
cat("Materialized frozen transform inputs at ", root, "\n", sep = "")

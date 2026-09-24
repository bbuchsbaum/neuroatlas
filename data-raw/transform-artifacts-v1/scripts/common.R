require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required package is not installed: ", package, call. = FALSE)
  }
}

package_receipt <- function(package) {
  require_namespace(package)
  description <- utils::packageDescription(package)
  remote_sha <- description$RemoteSha
  if (is.null(remote_sha) || !is.character(remote_sha) ||
      length(remote_sha) != 1L || !nzchar(remote_sha)) {
    remote_sha <- NA_character_
  }
  list(
    package = package,
    version = as.character(utils::packageVersion(package)),
    remote_sha = remote_sha
  )
}

software_receipts <- function(packages) {
  if (!is.character(packages) || length(packages) < 1L || any(!nzchar(packages))) {
    stop("packages must be a non-empty character vector.", call. = FALSE)
  }
  receipts <- lapply(packages, package_receipt)
  names(receipts) <- packages
  c(
    receipts,
    list(r = list(version = R.version$version.string, platform = R.version$platform))
  )
}

script_root <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", arguments, value = TRUE)
  if (length(file_arg) != 1L) {
    stop("Invoke this script with Rscript so its location is known.", call. = FALSE)
  }
  normalizePath(dirname(sub("^--file=", "", file_arg)), mustWork = TRUE)
}

artifact_root <- function() {
  script_root()
}

repo_root <- function() {
  configured <- Sys.getenv("NEUROATLAS_REPO_ROOT", unset = "")
  if (nzchar(configured)) {
    return(normalizePath(configured, mustWork = TRUE))
  }
  normalizePath(file.path(artifact_root(), "..", ".."), mustWork = TRUE)
}

work_root <- function(create = FALSE) {
  configured <- Sys.getenv("NEUROATLAS_TRANSFORM_WORK_ROOT", unset = "")
  path <- if (nzchar(configured)) configured else {
    file.path(repo_root(), "data-raw", "transform-artifacts-v1-work")
  }
  if (isTRUE(create)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, mustWork = isTRUE(create))
}

read_json <- function(path) {
  require_namespace("jsonlite")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

write_json <- function(value, path) {
  require_namespace("jsonlite")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", digits = NA)
  invisible(path)
}

sha256_file <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("Cannot hash missing regular file: ", path, call. = FALSE)
  }
  require_namespace("digest")
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

assert_release_software <- function(route) {
  for (package in names(route$registration$software_commits)) {
    expected <- route$registration$software_commits[[package]]
    actual <- package_receipt(package)$remote_sha
    if (!is.character(expected) || !grepl("^[0-9a-f]{40}$", expected) ||
        !identical(expected, actual)) {
      stop("Unpinned or unexpected ", package, " source revision: ", actual,
           "; expected ", expected, call. = FALSE)
    }
  }
  invisible(TRUE)
}

file_receipt <- function(path) {
  list(
    path = normalizePath(path, mustWork = TRUE),
    bytes = unname(file.info(path)$size),
    sha256 = sha256_file(path)
  )
}

assert_file_receipt <- function(path, receipt) {
  actual_bytes <- unname(file.info(path)$size)
  if (!identical(as.numeric(actual_bytes), as.numeric(receipt$bytes))) {
    stop("Byte mismatch for ", path, ": expected ", receipt$bytes,
         ", found ", actual_bytes, call. = FALSE)
  }
  actual_sha <- sha256_file(path)
  if (!identical(actual_sha, receipt$sha256)) {
    stop("SHA-256 mismatch for ", path, call. = FALSE)
  }
  invisible(path)
}

routes_definition <- function() {
  read_json(file.path(artifact_root(), "routes.json"))
}

route_by_id <- function(route_id, routes = routes_definition()) {
  hits <- Filter(function(route) identical(route$route_id, route_id), routes$routes)
  if (length(hits) != 1L) {
    stop("Expected exactly one route with route_id=", route_id, call. = FALSE)
  }
  hits[[1L]]
}

route_input_specs <- function(route) {
  list(
    source_image = c(route$source$image, list(template = route$source$template)),
    source_mask = c(route$source$mask, list(template = route$source$template)),
    source_qa_labels = c(route$source$qa_labels, list(template = route$source$template)),
    target_image = c(route$target$image, list(template = route$target$template)),
    target_mask = c(route$target$mask, list(template = route$target$template)),
    target_qa_labels = c(route$target$qa_labels, list(template = route$target$template))
  )
}

materialized_input_path <- function(spec, root = work_root(create = FALSE)) {
  file.path(root, "inputs", spec$relpath)
}

materialize_input <- function(spec, root = work_root(create = TRUE)) {
  require_namespace("templateflow")
  query <- c(list(template = spec$template, read = FALSE), spec$query)
  source <- do.call(templateflow::tf_get, query)
  assert_file_receipt(source, spec)
  destination <- materialized_input_path(spec, root)
  if (file.exists(destination)) {
    assert_file_receipt(destination, spec)
    return(normalizePath(destination, mustWork = TRUE))
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, destination, copy.date = TRUE)) {
    stop("Could not copy frozen input to ", destination, call. = FALSE)
  }
  assert_file_receipt(destination, spec)
  normalizePath(destination, mustWork = TRUE)
}

read_campaign_json <- function(name, required = TRUE) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    if (required) stop("Campaign wrapper did not provide ", name, call. = FALSE)
    return(list())
  }
  require_namespace("jsonlite")
  jsonlite::fromJSON(value, simplifyVector = FALSE)
}

route_id_from_args_or_campaign <- function() {
  params <- read_campaign_json("RS_PARAMS_JSON", required = FALSE)
  if (!is.null(params$route_id)) return(as.character(params$route_id))
  arguments <- commandArgs(trailingOnly = TRUE)
  option <- "--route-id="
  hit <- grep(paste0("^", option), arguments, value = TRUE)
  if (length(hit) == 1L) return(sub(paste0("^", option), "", hit))
  stop("Provide route_id through RS_PARAMS_JSON or --route-id=...", call. = FALSE)
}

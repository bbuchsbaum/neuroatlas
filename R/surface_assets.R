#' Fetch Exact Pinned Surface Registration Geometry
#'
#' Downloads checksum-locked registration spheres, cortical masks and vertex
#' areas from their original providers. Only the pinned fsaverage 164k and fsLR
#' 32k domains are supported. The fsLR sphere is in fsaverage correspondence.
#' Inputs retain their upstream licenses, independently of neuroatlas's license;
#' see `extdata/surface-assets-LICENSES.md` in the installed package.
#'
#' @param template Exact family, `"fsaverage"` or `"fsLR"`.
#' @param density `"164k"` for fsaverage or `"32k"` for fsLR.
#' @param hemisphere `"L"` or `"R"`.
#' @param cache_dir Dedicated transform cache directory.
#' @param download Allow downloads from pinned upstream URLs.
#' @param offline Use only checksum-verified local files.
#' @return A `SurfaceGeometry` with exact domain identity, pinned input hashes,
#'   cortical mask, and upstream file provenance.
#' @examples
#' \dontrun{
#' left <- get_surface_geometry("fsaverage", "164k", "L")
#' target <- get_surface_geometry("fsLR", "32k", "L")
#' transform <- get_template_transform(left, target)
#' x <- surface_data(rep(0.25, left$domain$n_vertices), left$domain)
#' y <- apply_template_transform(x, transform)
#' }
#' @export
get_surface_geometry <- function(
  template,
  density,
  hemisphere,
  cache_dir = transform_cache_path(),
  download = TRUE,
  offline = FALSE
) {
  for (flag in list(download, offline)) {
    assertthat::assert_that(is.logical(flag), length(flag) == 1L, !is.na(flag))
  }
  for (value in list(template, density, hemisphere)) {
    assertthat::assert_that(
      is.character(value),
      length(value) == 1L,
      !is.na(value),
      nzchar(value)
    )
  }
  if (!requireNamespace("gifti", quietly = TRUE)) {
    stop("Surface geometry requires the optional 'gifti' package.")
  }
  catalog <- .surface_input_json("surface-domains-v1.json")
  name <- paste(template, density, hemisphere, sep = "-")
  entry <- catalog$domains[[name]]
  if (is.null(entry)) stop("No pinned exact surface domain: ", name)
  lock <- .surface_input_json("surface-inputs-v1.json")
  if (
    !identical(
      digest::digest(
        file = .surface_input_path("surface-inputs-v1.json"),
        algo = "sha256"
      ),
      catalog$input_lock_sha256
    )
  ) {
    stop("Surface input manifest does not match its pinned identity.")
  }
  inputs <- lapply(
    entry$assets,
    function(path) {
      a <- Filter(function(a) identical(a$path, path), lock$assets)
      if (length(a) != 1L) stop("Missing or ambiguous surface asset: ", path)
      .read_locked_surface_input(a[[1L]], lock, cache_dir, download, offline)
    }
  )
  d <- entry$domain
  sphere <- inputs$sphere$data$pointset
  faces <- inputs$sphere$data$triangle
  cortex <- as.logical(as.vector(inputs$mask$data[[1L]]))
  area <- as.numeric(inputs$area$data[[1L]])
  domain <- surface_domain(
    d$template,
    d$hemisphere,
    d$density,
    sphere,
    faces,
    cortex,
    d$registration,
    d$revision,
    vertex_area = area
  )
  if (!identical(domain$id, d$id)) stop("Pinned surface domain identity mismatch.")
  result <- surface_geometry(domain, sphere, faces, cortex)
  result$files <- entry$assets
  result$input_lock_sha256 <- catalog$input_lock_sha256
  result
}

.surface_input_path <- function(name) {
  file.path(dirname(.transform_registry_path()), name)
}

.surface_input_json <- function(name) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Pinned surface input manifests require optional 'jsonlite'.")
  }
  jsonlite::read_json(.surface_input_path(name))
}

.locked_surface_artifact <- function(a, format, version = "surface-inputs-v1") {
  data.frame(
    artifact_id = a$sha256,
    artifact_version = version,
    provider = "neuroatlas",
    url = a$url,
    sha256 = a$sha256,
    size_bytes = a$size_bytes,
    format = format,
    qualification = "checksum_locked",
    qualification_scope = "pinned_surface_input",
    status = "available",
    stringsAsFactors = FALSE
  )
}

.read_locked_surface_input <- function(a, lock, cache_dir, download, offline) {
  parse <- function(path) {
    g <- gifti::readgii(path)
    if (!is.null(a$hemisphere)) {
      actual <- g$file_meta["AnatomicalStructurePrimary"]
      expected <- if (a$hemisphere == "L") "CortexLeft" else "CortexRight"
      if (length(actual) && !is.na(actual) && unname(actual) != expected) {
        stop("Pinned GIFTI hemisphere metadata mismatch.")
      }
    }
    g
  }
  if (is.null(a$archive)) {
    return(
      .fetch_transform_artifact(
        .locked_surface_artifact(a, "surface_gifti"),
        cache_dir,
        download,
        offline,
        .use = parse
      )
    )
  }
  archives <- Filter(function(x) identical(x$id, a$archive), lock$archives)
  if (length(archives) != 1L) stop("Missing or ambiguous pinned archive.")
  .fetch_transform_artifact(
    .locked_surface_artifact(archives[[1L]], "surface_tar"),
    cache_dir,
    download,
    offline,
    .use = function(path) {
      bytes <- .surface_tar_member(path, a$member)
      if (
        length(bytes) != a$size_bytes ||
          digest::digest(bytes, algo = "sha256", serialize = FALSE) != a$sha256
      ) {
        stop("Pinned archive member failed its integrity check.")
      }
      tmp <- tempfile(fileext = ".gii")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(bytes, tmp)
      parse(tmp)
    }
  )
}

# Read one exact regular-file tar member in memory. Never extract archive paths.
.surface_tar_member <- function(path, member) {
  connection <- gzfile(path, "rb")
  on.exit(close(connection), add = TRUE)
  string <- function(bytes) {
    rawToChar(
      bytes[seq_len(
        match(
          as.raw(0),
          bytes,
          nomatch = length(bytes) + 1L
        ) - 1L
      )]
    )
  }
  repeat {
    header <- readBin(connection, "raw", n = 512L)
    if (length(header) != 512L || all(header == as.raw(0))) break
    name <- string(header[1:100])
    prefix <- string(header[346:500])
    if (nzchar(prefix)) name <- paste(prefix, name, sep = "/")
    size <- strtoi(trimws(string(header[125:136])), base = 8L)
    if (is.na(size) || size < 0) stop("Invalid pinned tar member size.")
    bytes <- readBin(connection, "raw", n = size)
    if (length(bytes) != size) stop("Truncated pinned tar member.")
    padding <- (512L - size %% 512L) %% 512L
    if (padding) readBin(connection, "raw", n = padding)
    if (identical(name, member)) {
      if (!header[[157L]] %in% c(as.raw(0), charToRaw("0"))) {
        stop("Pinned archive member must be a regular file.")
      }
      return(bytes)
    }
  }
  stop("Pinned archive member is unavailable: ", member)
}

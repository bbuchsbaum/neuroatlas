#' Atlas Provenance Accessors
#'
#' @description
#' Access structured provenance metadata for atlas objects, including the
#' canonical atlas identity, upstream artifacts, and processing history.
#'
#' @name atlas_provenance
NULL


#' Get Full Atlas Provenance
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of class `"atlas_provenance"` with fields:
#'   \describe{
#'     \item{ref}{Canonical \code{\link{atlas_ref}()} identity metadata.}
#'     \item{artifacts}{A tibble describing upstream files/resources.}
#'     \item{history}{A tibble describing processing steps applied in
#'       \code{neuroatlas}.}
#'   }
#' @export
atlas_provenance <- function(x, ...) {
  UseMethod("atlas_provenance")
}

#' @rdname atlas_provenance
#' @export
atlas_provenance.atlas <- function(x, ...) {
  structure(
    list(
      ref = atlas_ref(x),
      artifacts = atlas_artifacts(x),
      history = atlas_history(x)
    ),
    class = c("atlas_provenance", "list")
  )
}

#' @rdname atlas_provenance
#' @export
atlas_provenance.default <- function(x, ...) {
  stop("No atlas_provenance() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' Get Atlas Artifact Metadata
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with one row per upstream artifact used to construct the
#'   atlas.
#' @export
atlas_artifacts <- function(x, ...) {
  UseMethod("atlas_artifacts")
}

#' @rdname atlas_provenance
#' @export
atlas_artifacts.atlas <- function(x, ...) {
  if (!is.null(x$metadata)) return(atlas_metadata(x)$artifacts)
  if (is.null(x$atlas_artifacts)) {
    return(.empty_atlas_artifacts())
  }
  .normalize_atlas_artifacts(x$atlas_artifacts)
}

#' @rdname atlas_provenance
#' @export
atlas_artifacts.default <- function(x, ...) {
  stop("No atlas_artifacts() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' Get Atlas Processing History
#'
#' @param x An atlas object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble with one row per processing step tracked by
#'   \code{neuroatlas}.
#' @export
atlas_history <- function(x, ...) {
  UseMethod("atlas_history")
}

#' @rdname atlas_provenance
#' @export
atlas_history.atlas <- function(x, ...) {
  if (!is.null(x$metadata)) return(atlas_metadata(x)$history)
  if (is.null(x$atlas_history)) {
    return(.empty_atlas_history())
  }
  .normalize_atlas_history(x$atlas_history)
}

#' @rdname atlas_provenance
#' @export
atlas_history.default <- function(x, ...) {
  stop("No atlas_history() method for objects of class: ",
       paste(class(x), collapse = ", "))
}


#' @rdname print-methods
#' @param x An \code{atlas_provenance} object.
#' @param ... Unused.
#' @return Invisibly returns \code{x}.
#' @export
print.atlas_provenance <- function(x, ...) {
  cat("<atlas_provenance>\n")
  cat("  family:", x$ref$family, "\n")
  cat("  model:", x$ref$model, "\n")
  cat("  artifacts:", nrow(x$artifacts), "\n")
  cat("  history steps:", nrow(x$history), "\n")
  invisible(x)
}


#' @keywords internal
#' @noRd
.empty_atlas_artifacts <- function() {
  tibble::tibble(
    role = character(),
    family = character(),
    model = character(),
    variant = character(),
    source_name = character(),
    source_url = character(),
    source_ref = character(),
    source_version = character(),
    citation_doi = character(),
    license = character(),
    license_url = character(),
    file_name = character(),
    local_path = character(),
    sha256 = character(),
    checksum = character(),
    checksum_algorithm = character(),
    checksum_basis = character(),
    template_space = character(),
    coord_space = character(),
    resolution = character(),
    density = character(),
    parcels = character(),
    networks = character(),
    hemi = character(),
    lineage = character(),
    confidence = character(),
    notes = character()
  )
}


#' @keywords internal
#' @noRd
.empty_atlas_history <- function() {
  tibble::tibble(
    step = integer(),
    action = character(),
    representation = character(),
    from_template_space = character(),
    to_template_space = character(),
    from_coord_space = character(),
    to_coord_space = character(),
    status = character(),
    confidence = character(),
    details = character(),
    parameters = list(),
    software_version = character()
  )
}


#' @keywords internal
#' @noRd
.normalize_atlas_artifacts <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  template <- .empty_atlas_artifacts()

  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- rep(NA_character_, nrow(out))
    }
    out[[nm]] <- as.character(out[[nm]])
  }

  tibble::as_tibble(out[, names(template), drop = FALSE])
}


#' @keywords internal
#' @noRd
.normalize_atlas_history <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  template <- .empty_atlas_history()

  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- if (nm == "step") integer(nrow(out)) else {
        if (nm == "parameters") rep(list(list()), nrow(out)) else {
          rep(NA_character_, nrow(out))
        }
      }
    }
  }

  out$step <- as.integer(out$step)
  for (nm in setdiff(names(template), c("step", "parameters"))) {
    out[[nm]] <- as.character(out[[nm]])
  }

  tibble::as_tibble(out[, names(template), drop = FALSE])
}


#' @keywords internal
#' @noRd
.new_atlas_artifact <- function(role,
                                family,
                                model,
                                variant = NA_character_,
                                source_name = NA_character_,
                                source_url = NA_character_,
                                source_ref = NA_character_,
                                source_version = NA_character_,
                                citation_doi = NA_character_,
                                license = NA_character_,
                                license_url = NA_character_,
                                file_name = NA_character_,
                                local_path = NA_character_,
                                sha256 = NA_character_,
                                template_space = NA_character_,
                                coord_space = NA_character_,
                                resolution = NA_character_,
                                density = NA_character_,
                                parcels = NA_character_,
                                networks = NA_character_,
                                hemi = NA_character_,
                                lineage = NA_character_,
                                confidence = NA_character_,
                                notes = NA_character_) {
  terms <- .artifact_license(source_name, family)
  if (is.na(license)) license <- terms[["license"]]
  if (is.na(license_url)) license_url <- terms[["license_url"]]
  receipt <- .file_receipt(local_path)
  if (is.na(receipt$checksum) && identical(source_name, "neuroatlas")) {
    bundled <- .bundled_file_receipt(file_name)
    if (!is.null(bundled)) receipt <- bundled
  }
  if (is.na(sha256)) sha256 <- receipt$sha256
  tibble::tibble(
    role = as.character(role),
    family = as.character(family),
    model = as.character(model),
    variant = as.character(variant),
    source_name = as.character(source_name),
    source_url = as.character(source_url),
    source_ref = as.character(source_ref),
    source_version = as.character(source_version),
    citation_doi = as.character(citation_doi),
    license = as.character(license),
    license_url = as.character(license_url),
    file_name = as.character(file_name),
    local_path = receipt$local_path,
    sha256 = as.character(sha256),
    checksum = receipt$checksum,
    checksum_algorithm = receipt$checksum_algorithm,
    checksum_basis = receipt$checksum_basis,
    template_space = as.character(template_space),
    coord_space = as.character(coord_space),
    resolution = as.character(resolution),
    density = as.character(density),
    parcels = as.character(parcels),
    networks = as.character(networks),
    hemi = as.character(hemi),
    lineage = as.character(lineage),
    confidence = as.character(confidence),
    notes = as.character(notes)
  )
}


#' @keywords internal
#' @noRd
.new_atlas_history <- function(action,
                               representation,
                               from_template_space = NA_character_,
                               to_template_space = NA_character_,
                               from_coord_space = NA_character_,
                               to_coord_space = NA_character_,
                               status = "available",
                               confidence = NA_character_,
                               details = NA_character_,
                               parameters = list()) {
  tibble::tibble(
    step = NA_integer_,
    action = as.character(action),
    representation = as.character(representation),
    from_template_space = as.character(from_template_space),
    to_template_space = as.character(to_template_space),
    from_coord_space = as.character(from_coord_space),
    to_coord_space = as.character(to_coord_space),
    status = as.character(status),
    confidence = as.character(confidence),
    details = as.character(details),
    parameters = list(parameters),
    software_version = as.character(utils::packageVersion("neuroatlas"))
  )
}


#' @keywords internal
#' @noRd
.attach_atlas_provenance <- function(x, artifacts = NULL, history = NULL,
                                    metadata_inputs = list()) {
  x$atlas_artifacts <- if (is.null(artifacts)) {
    .empty_atlas_artifacts()
  } else {
    .normalize_atlas_artifacts(artifacts)
  }

  x$atlas_history <- if (is.null(history)) {
    .empty_atlas_history()
  } else {
    hist <- .normalize_atlas_history(history)
    hist$step <- seq_len(nrow(hist))
    hist
  }

  meta <- .enrich_atlas_metadata(.build_atlas_metadata(x), metadata_inputs)
  .store_atlas_metadata(x, meta)
}


#' @keywords internal
#' @noRd
.append_atlas_history <- function(x,
                                  action,
                                  representation = NULL,
                                  from_template_space = NA_character_,
                                  to_template_space = NA_character_,
                                  from_coord_space = NA_character_,
                                  to_coord_space = NA_character_,
                                  status = "available",
                                  confidence = NA_character_,
                                  details = NA_character_,
                                  parameters = list()) {
  if (is.null(representation)) {
    representation <- if (inherits(x, "surfatlas")) "surface" else "volume"
  }

  row <- .new_atlas_history(
    action = action,
    representation = representation,
    from_template_space = from_template_space,
    to_template_space = to_template_space,
    from_coord_space = from_coord_space,
    to_coord_space = to_coord_space,
    status = status,
    confidence = confidence,
    details = details,
    parameters = parameters
  )

  meta <- atlas_metadata(x)
  hist <- meta$history
  hist <- dplyr::bind_rows(hist, row)
  hist$step <- seq_len(nrow(hist))
  meta$history <- hist
  .refresh_atlas_metadata(x, meta)
}

# Hash the exact bytes read, at load time. R versions without tools::sha256sum
# retain an explicitly labelled MD5 receipt; no new dependency is required.
.file_receipt <- function(path) {
  ret <- list(local_path = NA_character_, sha256 = NA_character_,
              checksum = NA_character_, checksum_algorithm = NA_character_,
              checksum_basis = NA_character_)
  if (length(path) != 1L || is.na(path) || !file.exists(path) ||
      dir.exists(path)) return(ret)
  ret$local_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  sha <- "sha256sum" %in% getNamespaceExports("tools")
  hash <- if (sha) getExportedValue("tools", "sha256sum") else tools::md5sum
  ret$checksum <- unname(hash(path))
  ret$checksum_algorithm <- if (sha) "sha256" else "md5"
  ret$checksum_basis <- "read_file"
  if (sha) ret$sha256 <- ret$checksum
  ret
}

.bundled_file_receipt <- function(file_name) {
  path <- system.file("extdata", "resource_checksums.csv", package = "neuroatlas")
  if (!nzchar(path) || is.na(file_name)) return(NULL)
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE)
  row <- manifest[basename(manifest$resource) == file_name, , drop = FALSE]
  if (nrow(row) != 1L) return(NULL)
  list(local_path = NA_character_, sha256 = row$sha256, checksum = row$sha256,
        checksum_algorithm = "sha256", checksum_basis = "source_manifest")
}

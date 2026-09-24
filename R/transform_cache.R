#' Transform Artifact Cache Path
#'
#' Returns the directory reserved for verified transform artifacts. This helper
#' does not create the directory.
#'
#' @param cache_dir Base cache directory.
#'
#' @return A character path to the transform cache directory.
#' @export
transform_cache_path <- function(cache_dir = tools::R_user_dir("neuroatlas", "cache")) {
  file.path(cache_dir, "transforms")
}


#' Clear Verified Transform Artifacts
#'
#' Removes only transform artifact files owned by neuroatlas. The cache root is
#' retained, and a cache containing an active artifact lock is refused.
#'
#' @param artifact_version Optional immutable artifact version to remove.
#' @param cache_dir Transform cache directory, normally [transform_cache_path()].
#'
#' @return The number of artifact files removed, invisibly.
#' @export
clear_transform_cache <- function(artifact_version = NULL,
                                  cache_dir = transform_cache_path()) {
  root <- .transform_cache_root(cache_dir)
  if (!is.null(artifact_version)) .transform_cache_segment(artifact_version,
                                                            "artifact_version")
  if (!dir.exists(root)) return(invisible(0L))
  .with_transform_cache_lock(file.path(root, ".neuroatlas-cache.lock"),
    artifact = NULL, target = NULL, code = function() {
      .clear_transform_cache_unlocked(artifact_version, root)
    })
}

.clear_transform_cache_unlocked <- function(artifact_version, root) {
  if (.transform_cache_has_lock(root)) {
    stop("Cannot clear transform cache while an artifact lock is active.",
         call. = FALSE)
  }

  versions <- if (is.null(artifact_version)) {
    list.dirs(root, recursive = FALSE, full.names = TRUE)
  } else {
    file.path(root, artifact_version)
  }
  removed <- 0L
  for (version_dir in versions) {
    if (!dir.exists(version_dir) || .transform_cache_is_symlink(version_dir)) next
    version <- basename(version_dir)
    if (!.transform_cache_segment_ok(version)) next
    entries <- list.files(version_dir, full.names = TRUE, no.. = TRUE,
                           all.files = TRUE)
    if (any(grepl("^\\..+\\.lock$", basename(entries)))) {
      stop("Cannot clear transform cache while an artifact lock is active.",
           call. = FALSE)
    }
    artifacts <- entries[
      vapply(entries, function(path) {
        !dir.exists(path) && !.transform_cache_is_symlink(path) &&
          grepl("^[A-Za-z0-9][A-Za-z0-9._-]*\\.h5$", basename(path)) &&
          .transform_cache_owned(path)
      }, logical(1))
    ]
    for (path in artifacts) {
      if (unlink(path) != 0L) {
        stop("Failed to remove transform cache artifact: ", path, call. = FALSE)
      }
      unlink(paste0(path, ".neuroatlas-receipt"))
      removed <- removed + 1L
    }
    if (length(list.files(version_dir, all.files = TRUE, no.. = TRUE)) == 0L) {
      unlink(version_dir, recursive = FALSE)
    }
  }
  invisible(removed)
}


#' @keywords internal
#' @noRd
.fetch_transform_artifact <- function(artifact,
                                      cache_dir,
                                      download = TRUE,
                                      offline = FALSE,
                                      verify = TRUE,
                                      .use = identity) {
  root <- .transform_cache_root(cache_dir)
  if (!dir.exists(root) && (isTRUE(offline) || !isTRUE(download))) {
    return(.use(.fetch_transform_artifact_unlocked(
      artifact, root, download, offline, verify)))
  }
  .validate_transform_artifact(artifact)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  .with_transform_cache_lock(file.path(root, ".neuroatlas-cache.lock"),
    artifact = NULL, target = NULL, code = function() {
      .use(.fetch_transform_artifact_unlocked(
        artifact, root, download, offline, verify))
    })
}

.fetch_transform_artifact_unlocked <- function(artifact, cache_dir,
                                                download, offline, verify) {
  if (!isTRUE(verify)) {
    stop("Transform artifact integrity verification cannot be disabled.",
         call. = FALSE)
  }
  .validate_transform_artifact(artifact)
  root <- .transform_cache_root(cache_dir)
  version <- as.character(artifact$artifact_version[[1L]])
  artifact_id <- as.character(artifact$artifact_id[[1L]])
  target <- file.path(root, version, paste0(artifact_id, ".h5"))
  if (.transform_cache_is_symlink(dirname(target))) {
    stop("Unsafe symbolic link at transform cache version directory.", call. = FALSE)
  }

  if (file.exists(target)) {
    if (.transform_cache_is_symlink(target)) {
      stop("Unsafe symbolic link at transform cache artifact path.", call. = FALSE)
    }
    .verify_transform_artifact_file(target, artifact)
    return(normalizePath(target, mustWork = TRUE))
  }
  if (isTRUE(offline) || !isTRUE(download)) {
    stop("Verified transform artifact '", artifact_id,
         "' is unavailable in the local cache and download is disabled.",
         call. = FALSE)
  }

  version_dir <- dirname(target)
  if (!dir.exists(version_dir) && !dir.create(version_dir, recursive = TRUE,
                                                showWarnings = FALSE)) {
    stop("Could not create transform cache directory: ", version_dir,
         call. = FALSE)
  }
  if (.transform_cache_is_symlink(version_dir)) {
    stop("Unsafe symbolic link at transform cache version directory.",
         call. = FALSE)
  }
  lock <- file.path(version_dir, paste0(".", artifact_id, ".lock"))
  .with_transform_cache_lock(lock, artifact, target, function() {
    if (!is.null(target) && file.exists(target)) {
      if (.transform_cache_is_symlink(target)) {
        stop("Unsafe symbolic link at transform cache artifact path.",
             call. = FALSE)
      }
      .verify_transform_artifact_file(target, artifact)
      return(normalizePath(target, mustWork = TRUE))
    }
    tmp <- tempfile(paste0(".", artifact_id, "-"), tmpdir = version_dir,
                    fileext = ".part")
    keep <- FALSE
    on.exit(if (!keep && file.exists(tmp)) unlink(tmp), add = TRUE)
    .neuroatlas_download(
      artifact$url[[1L]], dest = tmp, min_size = 0L,
      description = paste("transform artifact", artifact_id)
    )
    .verify_transform_artifact_file(tmp, artifact)
    if (!file.rename(tmp, target)) {
      stop("Could not atomically publish transform cache artifact: ", target,
           call. = FALSE)
    }
    write.dcf(data.frame(
      owner = "neuroatlas-transform-cache-v1", artifact_id = artifact_id,
      file = basename(target), sha256 = artifact$sha256[[1L]],
      stringsAsFactors = FALSE
    ), paste0(target, ".neuroatlas-receipt"))
    keep <- TRUE
    normalizePath(target, mustWork = TRUE)
  })
}


#' @keywords internal
#' @noRd
.validate_transform_artifact <- function(artifact) {
  required <- c("artifact_id", "artifact_version", "provider", "url", "sha256",
                "size_bytes", "format", "qualification", "status")
  if (!is.data.frame(artifact) || nrow(artifact) != 1L ||
      !all(required %in% names(artifact))) {
    stop("A transform artifact must be a one-row data frame with the required ",
         "release fields.", call. = FALSE)
  }
  scalar <- function(name) {
    value <- artifact[[name]][[1L]]
    !is.na(value) && length(value) == 1L && nzchar(as.character(value))
  }
  if (!all(vapply(required[-6L], scalar, logical(1)))) {
    stop("Transform artifact release fields must be non-missing scalars.",
         call. = FALSE)
  }
  .transform_cache_segment(artifact$artifact_id[[1L]], "artifact_id")
  .transform_cache_segment(artifact$artifact_version[[1L]], "artifact_version")
  qualification <- as.character(artifact$qualification[[1L]])
  qualified <- identical(qualification, "passed") ||
    (identical(qualification, "runtime_verified") &&
     identical(as.character(artifact$provider[[1L]]), "templateflow") &&
     "qualification_scope" %in% names(artifact) &&
     identical(as.character(artifact$qualification_scope[[1L]]),
               "upstream_transform_application"))
  if (!identical(as.character(artifact$status[[1L]]), "available") ||
      !qualified ||
      !identical(as.character(artifact$format[[1L]]), "ants_h5")) {
    stop("Transform artifact is not an available, qualified ANTs H5 release.",
         call. = FALSE)
  }
  url <- as.character(artifact$url[[1L]])
  if (!grepl("^https://[^/?#]+/[^?#]+$", url) ||
      grepl("(^|[/_-])latest([/_-]|$)", url, ignore.case = TRUE)) {
    stop("Transform artifact URL must be an immutable HTTPS URL and cannot use ",
         "a latest alias.", call. = FALSE)
  }
  sha256 <- as.character(artifact$sha256[[1L]])
  if (!grepl("^[0-9a-f]{64}$", sha256)) {
    stop("Transform artifact sha256 must be a full lowercase SHA-256 digest.",
         call. = FALSE)
  }
  size <- suppressWarnings(as.numeric(artifact$size_bytes[[1L]]))
  if (!is.finite(size) || size <= 0 || size != floor(size)) {
    stop("Transform artifact size_bytes must be a positive integer.", call. = FALSE)
  }
  invisible(TRUE)
}


#' @keywords internal
#' @noRd
.verify_transform_artifact_file <- function(path, artifact) {
  expected_size <- as.numeric(artifact$size_bytes[[1L]])
  actual_size <- file.info(path)$size
  actual_sha <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  if (!isTRUE(actual_size == expected_size) ||
      !identical(actual_sha, as.character(artifact$sha256[[1L]]))) {
    stop("Transform cache artifact failed its release integrity check: ", path,
         call. = FALSE)
  }
  invisible(TRUE)
}


#' @keywords internal
#' @noRd
.with_transform_cache_lock <- function(lock, artifact, target, code) {
  timeout <- getOption("neuroatlas.transform_cache_lock_timeout", 5)
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) ||
      timeout < 0) stop("Transform cache lock timeout must be finite and nonnegative.")
  deadline <- proc.time()[["elapsed"]] + timeout
  acquired <- FALSE
  while (!acquired) {
    acquired <- dir.create(lock, showWarnings = FALSE)
    if (acquired) break
    if (!is.null(target) && file.exists(target)) {
      .verify_transform_artifact_file(target, artifact)
      return(normalizePath(target, mustWork = TRUE))
    }
    if (proc.time()[["elapsed"]] >= deadline) {
      stop("Timed out waiting for transform artifact cache lock: ", lock,
           call. = FALSE)
    }
    Sys.sleep(0.05)
  }
  on.exit(if (acquired && dir.exists(lock)) unlink(lock, recursive = TRUE), add = TRUE)
  code()
}


#' @keywords internal
#' @noRd
.transform_cache_root <- function(cache_dir) {
  if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir) ||
      !nzchar(cache_dir)) {
    stop("cache_dir must be a non-empty character scalar.", call. = FALSE)
  }
  if (.transform_cache_is_symlink(cache_dir)) {
    stop("Unsafe symbolic link at transform cache directory.", call. = FALSE)
  }
  cache_dir <- path.expand(cache_dir)
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", cache_dir)) {
    cache_dir <- file.path(getwd(), cache_dir)
  }
  root <- normalizePath(cache_dir, mustWork = FALSE)
  components <- strsplit(root, "/", fixed = TRUE)[[1L]]
  if (identical(root, "/") || .transform_cache_is_symlink(root) ||
      any(tolower(components) == "templateflow")) {
    stop("Unsafe transform cache directory.", call. = FALSE)
  }
  root
}


#' @keywords internal
#' @noRd
.transform_cache_segment_ok <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", x)
}


#' @keywords internal
#' @noRd
.transform_cache_segment <- function(x, name) {
  if (!.transform_cache_segment_ok(x)) {
    stop(name, " must be a safe single path segment.", call. = FALSE)
  }
  invisible(x)
}


#' @keywords internal
#' @noRd
.transform_cache_is_symlink <- function(path) {
  link <- Sys.readlink(path)
  is.character(path) && length(path) == 1L && !is.na(link) && nzchar(link)
}

.transform_cache_owned <- function(path) {
  receipt <- paste0(path, ".neuroatlas-receipt")
  if (!file.exists(receipt) || .transform_cache_is_symlink(receipt)) return(FALSE)
  value <- tryCatch(read.dcf(receipt), error = function(e) NULL)
  !is.null(value) && nrow(value) == 1L &&
    all(c("owner", "file") %in% colnames(value)) &&
    identical(unname(value[1L, "owner"]), "neuroatlas-transform-cache-v1") &&
    identical(unname(value[1L, "file"]), basename(path))
}


#' @keywords internal
#' @noRd
.transform_cache_has_lock <- function(root) {
  locks <- basename(list.dirs(root, recursive = TRUE, full.names = TRUE))
  any(grepl("^\\..+\\.lock$", locks) & locks != ".neuroatlas-cache.lock")
}

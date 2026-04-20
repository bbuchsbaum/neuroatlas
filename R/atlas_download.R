#' Download Helpers for Atlas Loaders
#'
#' @description
#' Small, centralised wrappers around the `downloader` package used by atlas
#' loaders. They replace ad-hoc `tryCatch(..., error = function(e) NULL)` blocks
#' that silently hid failures, and instead produce typed, informative
#' conditions via [cli::cli_abort()].
#'
#' Two entry points are provided:
#'
#' * [.neuroatlas_download()] — hard download: throws
#'   `neuroatlas_error_download` on failure. Use this when the caller cannot
#'   recover from a missing file.
#' * [.neuroatlas_try_download()] — soft download: returns a small list
#'   describing the outcome (path, success, error, lfs_pointer). Use this when
#'   the caller wants to implement a fallback strategy.
#'
#' Both helpers detect Git LFS pointer files (text stubs of size ~133 bytes)
#' and treat them as failure modes, so loaders don't silently accept a stub
#' in place of the real NIfTI.
#'
#' `downloader` is declared in `Suggests`; the helpers call
#' [.require_suggest()] up front and raise a
#' `neuroatlas_error_missing_suggest` condition when the package isn't
#' installed, so atlases that don't need network access (ASEG, Olsen MTL)
#' still load without the dependency.
#'
#' @keywords internal
#' @name atlas_download
NULL


#' Ensure a destination directory exists (used by download helpers).
#'
#' @keywords internal
#' @noRd
.ensure_dir <- function(path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}


#' Detect a Git LFS pointer file.
#'
#' LFS pointer files begin with `version https://git-lfs.github.com/spec/v1`
#' and are ~133 bytes. Atlas loaders treat these as a failure mode because
#' `downloader::download()` returns without error even when the real binary
#' blob wasn't delivered.
#'
#' @keywords internal
#' @noRd
.is_lfs_pointer <- function(path) {
  if (!file.exists(path)) return(FALSE)
  size_ok <- file.info(path)$size < 1024L
  if (!size_ok) return(FALSE)
  hdr <- tryCatch(
    readLines(path, n = 1L, warn = FALSE),
    error = function(e) ""
  )
  identical(hdr, "version https://git-lfs.github.com/spec/v1")
}


#' Download an Atlas Asset (hard).
#'
#' @param url URL to fetch.
#' @param dest Destination path. If `NULL`, a path in [tempdir()] based on
#'   `basename(url)` is used.
#' @param mode Passed through to [downloader::download()] (default `"wb"`).
#' @param quiet Passed through to [downloader::download()].
#' @param min_size Minimum acceptable file size in bytes. Anything smaller is
#'   treated as a failed/corrupt download (typical LFS pointer stubs are well
#'   under this threshold).
#' @param description Short human-readable description of the asset, used in
#'   error messages (e.g. `"Schaefer volume"`).
#'
#' @return The destination path (invisibly on success).
#' @keywords internal
.neuroatlas_download <- function(url,
                                 dest = NULL,
                                 mode = "wb",
                                 quiet = TRUE,
                                 min_size = 1024L,
                                 description = "atlas asset") {
  .require_suggest("downloader", feature = "downloading atlas assets")

  if (is.null(dest)) {
    dest <- file.path(tempdir(), basename(url))
  }
  .ensure_dir(dest)

  status <- tryCatch(
    downloader::download(url, dest, mode = mode, quiet = quiet),
    error = function(e) e
  )

  if (inherits(status, "condition")) {
    cli::cli_abort(
      c(
        "Failed to download {description}.",
        "i" = "URL:  {.url {url}}",
        "x" = "{conditionMessage(status)}"
      ),
      class = c("neuroatlas_error_download", "neuroatlas_error"),
      parent = status
    )
  }

  if (!file.exists(dest)) {
    cli::cli_abort(
      c(
        "Download of {description} reported success but no file was written.",
        "i" = "URL:  {.url {url}}",
        "i" = "Dest: {.path {dest}}"
      ),
      class = c("neuroatlas_error_download", "neuroatlas_error")
    )
  }

  if (.is_lfs_pointer(dest)) {
    cli::cli_abort(
      c(
        "Downloaded {description} is a Git LFS pointer stub, not the real file.",
        "i" = "URL:  {.url {url}}",
        "i" = "The upstream mirror may be serving an unresolved LFS pointer."
      ),
      class = c("neuroatlas_error_lfs_pointer",
                "neuroatlas_error_download",
                "neuroatlas_error")
    )
  }

  if (file.info(dest)$size < min_size) {
    cli::cli_abort(
      c(
        "Downloaded {description} is smaller than expected ({file.info(dest)$size} bytes).",
        "i" = "URL:  {.url {url}}",
        "i" = "Minimum expected size: {min_size} bytes."
      ),
      class = c("neuroatlas_error_download", "neuroatlas_error")
    )
  }

  invisible(dest)
}


#' Download an Atlas Asset (soft).
#'
#' Wraps [.neuroatlas_download()] but returns a status list instead of
#' throwing, so callers can implement fallback strategies while still having
#' access to the full error condition (rather than a silent `NULL`).
#'
#' @return A list with fields:
#'   * `ok`: `TRUE` on success.
#'   * `path`: path to the downloaded file (or `NULL`).
#'   * `error`: the captured error condition on failure (or `NULL`).
#'   * `lfs_pointer`: `TRUE` if the file looked like a Git LFS pointer stub.
#' @keywords internal
.neuroatlas_try_download <- function(url,
                                     dest = NULL,
                                     mode = "wb",
                                     quiet = TRUE,
                                     min_size = 1024L,
                                     description = "atlas asset") {
  result <- tryCatch(
    {
      path <- .neuroatlas_download(
        url = url, dest = dest, mode = mode, quiet = quiet,
        min_size = min_size, description = description
      )
      list(ok = TRUE, path = path, error = NULL, lfs_pointer = FALSE)
    },
    neuroatlas_error_download = function(e) {
      list(
        ok = FALSE,
        path = NULL,
        error = e,
        lfs_pointer = inherits(e, "neuroatlas_error_lfs_pointer")
      )
    },
    error = function(e) {
      list(ok = FALSE, path = NULL, error = e, lfs_pointer = FALSE)
    }
  )
  result
}

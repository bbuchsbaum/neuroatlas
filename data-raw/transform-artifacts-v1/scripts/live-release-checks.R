# Checks shared by the manual public-asset integration script and synthetic tests.
live_receipts_equal <- function(observed, expected) {
  is.list(observed) && is.list(expected) &&
    is.character(expected$sha256) && length(expected$sha256) == 1L &&
    grepl("^[0-9a-f]{64}$", expected$sha256) &&
    is.numeric(expected$bytes) && length(expected$bytes) == 1L &&
    is.finite(expected$bytes) && expected$bytes > 0 &&
    identical(observed$sha256, expected$sha256) &&
    identical(as.numeric(observed$bytes), as.numeric(expected$bytes))
}

live_assert_receipt <- function(path, expected) {
  if (!file.exists(path) || dir.exists(path)) stop("Missing reference: ", path)
  observed <- list(bytes = unname(file.info(path)$size),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE))
  if (!live_receipts_equal(observed, expected)) {
    stop("Reference does not match its qualified receipt: ", path)
  }
  invisible(TRUE)
}

live_assert_grid <- function(path, reference) {
  x <- RNifti::readNifti(path)
  y <- RNifti::readNifti(reference)
  if (!identical(as.integer(dim(x)), as.integer(dim(y))) ||
      !identical(as.numeric(RNifti::xform(x)), as.numeric(RNifti::xform(y)))) {
    stop("Native reference geometry differs from the qualified target: ", path)
  }
  invisible(TRUE)
}

live_label_disagreements <- function(observed, native, allowed) {
  if (!identical(dim(observed), dim(native)) ||
      !identical(dim(observed), dim(allowed)) ||
      any(!is.finite(observed)) || any(!is.finite(native)) ||
      any(!is.finite(allowed)) || any(!allowed %in% c(0, 1))) {
    stop("Invalid label comparison arrays.")
  }
  disagreement <- observed != native
  if (any(disagreement & allowed == 0)) {
    stop("Label disagreement outside the qualified tie/edge set.")
  }
  sum(disagreement)
}

#' @rdname reduce_atlas
#' @inheritParams reduce_atlas
#' @method reduce_atlas surfatlas
#' @export
reduce_atlas.surfatlas <- function(atlas, data_vol, stat_func, ...) {
  if (!is.numeric(data_vol) && !is.matrix(data_vol)) {
    stop("'data_vol' must be a numeric vector or matrix of surface data.")
  }
  if (!is.function(stat_func)) {
    stop("'stat_func' must be a function.")
  }

  lh_len <- length(atlas$lh_atlas@data)
  rh_len <- length(atlas$rh_atlas@data)
  tot_len <- lh_len + rh_len

  if (is.matrix(data_vol)) {
    if (nrow(data_vol) == tot_len) {
      lh_data <- data_vol[seq_len(lh_len), , drop = FALSE]
      rh_data <- data_vol[lh_len + seq_len(rh_len), , drop = FALSE]
    } else if (ncol(data_vol) == tot_len) {
      lh_data <- data_vol[, seq_len(lh_len), drop = FALSE]
      rh_data <- data_vol[, lh_len + seq_len(rh_len), drop = FALSE]
    } else {
      stop("dimensions of 'data_vol' do not match atlas vertices")
    }
    lh_vals <- neurosurf::extract_roi_data(lh_data, atlas$lh_atlas, fun = stat_func, ...)
    rh_vals <- neurosurf::extract_roi_data(rh_data, atlas$rh_atlas, fun = stat_func, ...)
    mat <- cbind(lh_vals, rh_vals)
    mat <- t(mat)
    res <- tibble::as_tibble(mat)
    res <- tibble::add_column(res, time = seq_len(nrow(res)), .before = TRUE)
  } else {
    if (length(data_vol) != tot_len) {
      stop("length of 'data_vol' must match number of atlas vertices")
    }
    lh_vals <- neurosurf::extract_roi_data(data_vol[seq_len(lh_len)], atlas$lh_atlas, fun = stat_func, ...)
    rh_vals <- neurosurf::extract_roi_data(data_vol[lh_len + seq_len(rh_len)], atlas$rh_atlas, fun = stat_func, ...)
    vals <- c(lh_vals, rh_vals)
    res <- tibble::as_tibble(as.list(vals))
  }
  res
}

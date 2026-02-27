#' Compute Connectivity Matrix from Atlas Parcellations
#'
#' Extracts mean time-series for each atlas region from a 4D NeuroVec and
#' computes pairwise correlations to produce a connectivity matrix.
#'
#' @param data_vol A \code{NeuroVec} (4D) containing the time-series data.
#' @param atlas An atlas object defining the parcellation.
#' @param method Character string specifying the correlation method:
#'   \code{"pearson"} (default), \code{"spearman"}, or \code{"partial"}.
#'   Partial correlation requires the \pkg{corpcor} package.
#' @param threshold Optional numeric value. If supplied, entries with
#'   \code{abs(r) < threshold} are set to zero.
#' @param stat_func Function used to summarise voxel values within each
#'   parcel (default: \code{mean}).
#' @param ... Additional arguments passed to \code{\link{reduce_atlas}}.
#'
#' @return A symmetric matrix of class \code{c("atlas_connectivity", "matrix")}
#'   with region labels as dimnames.
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(parcels = "100", networks = "7")
#' # data_vol is a 4D NeuroVec from an fMRI run
#' conn <- atlas_connectivity(data_vol, atlas)
#' conn_sparse <- atlas_connectivity(data_vol, atlas, threshold = 0.3)
#' }
#'
#' @seealso \code{\link{reduce_atlas}} for the underlying extraction,
#'   \code{\link{as_igraph.atlas_connectivity}} for graph conversion
#'
#' @export
atlas_connectivity <- function(data_vol, atlas,
                                method = c("pearson", "spearman", "partial"),
                                threshold = NULL, stat_func = mean, ...) {
  method <- match.arg(method)

  if (!methods::is(data_vol, "NeuroVec")) {
    stop("'data_vol' must be a NeuroVec (4D) object")
  }

  # Extract T x K matrix using reduce_atlas
  ts_wide <- reduce_atlas(atlas, data_vol, stat_func, ..., format = "wide")

  # Remove 'time' column if present
  if ("time" %in% names(ts_wide)) {
    ts_wide <- ts_wide[, names(ts_wide) != "time", drop = FALSE]
  }

  # Convert to matrix
  ts_mat <- as.matrix(ts_wide)

  if (nrow(ts_mat) < 3) {
    stop("Need at least 3 time points to compute correlations")
  }

  # Compute correlation matrix
  if (method == "partial") {
    if (!requireNamespace("corpcor", quietly = TRUE)) {
      stop("Package 'corpcor' is required for partial correlations. ",
           "Install it with install.packages('corpcor')")
    }
    # First compute regular correlation, then partial
    R <- stats::cor(ts_mat, use = "pairwise.complete.obs")
    conn_mat <- corpcor::cor2pcor(R)
    dimnames(conn_mat) <- dimnames(R)
  } else {
    conn_mat <- stats::cor(ts_mat, method = method, use = "pairwise.complete.obs")
  }

  # Apply threshold
  if (!is.null(threshold)) {
    assertthat::assert_that(is.numeric(threshold), length(threshold) == 1)
    conn_mat[abs(conn_mat) < threshold] <- 0
  }

  # Set diagonal to zero (no self-connections)
  diag(conn_mat) <- 0

  structure(conn_mat, class = c("atlas_connectivity", class(conn_mat)))
}

#' Convert Atlas Connectivity to igraph
#'
#' @param x An \code{atlas_connectivity} matrix.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Dispatches to methods.
#'
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @rdname as_igraph
#' @param weighted Logical. If \code{TRUE} (default), edge weights are
#'   the correlation values. If \code{FALSE}, a binary adjacency is used.
#' @export
as_igraph.atlas_connectivity <- function(x, weighted = TRUE, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install it with install.packages('igraph')")
  }

  mat <- unclass(x)

  if (!weighted) {
    mat[mat != 0] <- 1
  }

  igraph::graph_from_adjacency_matrix(
    mat,
    mode = "undirected",
    weighted = if (weighted) TRUE else NULL,
    diag = FALSE
  )
}

#' @export
print.atlas_connectivity <- function(x, ...) {
  dims <- dim(x)
  cat("Atlas Connectivity Matrix\n")
  cat("  Regions:", dims[1], "\n")
  n_edges <- sum(x[upper.tri(x)] != 0)
  cat("  Non-zero edges:", n_edges, "\n")
  cat("  Range: [", round(min(x[upper.tri(x)]), 3), ", ",
      round(max(x[upper.tri(x)]), 3), "]\n", sep = "")
  invisible(x)
}

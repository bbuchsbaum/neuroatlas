#' Spin Test for Spatial Correlation Significance
#'
#' @description
#' Tests whether the spatial correlation between two parcellated brain maps is
#' stronger than expected by chance, using the spin-test framework of
#' Alexander-Bloch et al. (2018). Parcel centroids on a sphere surface are
#' randomly rotated and reassigned to the nearest original centroid, generating
#' a null distribution of correlations.
#'
#' @param map1 Numeric vector of parcel values (length K, aligned to atlas parcels).
#' @param map2 Numeric vector of parcel values (length K, aligned to atlas parcels).
#' @param atlas A surface atlas object (class \code{"surfatlas"}) with
#'   \code{lh_atlas} and \code{rh_atlas} fields, or any atlas whose geometry
#'   can provide sphere coordinates.
#' @param n_perm Integer. Number of spin permutations. Default: 1000.
#' @param cor_method Character. Correlation method passed to \code{\link[stats]{cor}}.
#'   One of \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#'   Default: \code{"pearson"}.
#' @param sphere Optional list with elements \code{lh} and \code{rh}, each an
#'   N x 3 matrix of sphere vertex coordinates. If \code{NULL} (default),
#'   sphere coordinates are fetched via
#'   \code{\link{load_surface_template}(..., "sphere")}.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A list of class \code{"spin_test"} with:
#' \describe{
#'   \item{observed}{The observed correlation between \code{map1} and \code{map2}.}
#'   \item{null_distribution}{Numeric vector of length \code{n_perm} containing
#'     null correlations.}
#'   \item{p_value}{Two-sided p-value: proportion of null correlations with
#'     absolute value >= the observed absolute correlation.}
#'   \item{n_perm}{Number of permutations performed.}
#' }
#'
#' @details
#' The algorithm:
#' \enumerate{
#'   \item Compute parcel centroids on the sphere surface by averaging the
#'     sphere coordinates of vertices belonging to each parcel.
#'   \item For each permutation, generate a uniform random 3D rotation matrix
#'     (from the Haar measure on SO(3)), rotate all centroids, then reassign
#'     each rotated centroid to its nearest original centroid using
#'     \code{\link[Rnanoflann]{nn}}.
#'   \item Compute the correlation of \code{map1} with the permuted
#'     \code{map2} (values shuffled according to the reassignment).
#'   \item The p-value is \code{(sum(abs(null) >= abs(observed)) + 1) /
#'     (n_perm + 1)}.
#' }
#'
#' @references
#' Alexander-Bloch, A. F., et al. (2018). On testing for spatial
#' correspondence between maps of human brain structure and function.
#' \emph{NeuroImage}, 178, 540-551.
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_surfatlas(parcels = "100", networks = "7")
#' K <- length(atlas$ids)
#' map1 <- rnorm(K)
#' map2 <- map1 + rnorm(K, sd = 0.5)
#' res  <- spin_test(map1, map2, atlas, n_perm = 500, seed = 42)
#' print(res)
#' }
#'
#' @export
spin_test <- function(map1, map2, atlas,
                      n_perm = 1000L,
                      cor_method = c("pearson", "spearman", "kendall"),
                      sphere = NULL,
                      seed = NULL) {

  cor_method <- match.arg(cor_method)
  n_perm <- as.integer(n_perm)

  K <- length(atlas$ids)
  if (length(map1) != K) {
    stop("length(map1) = ", length(map1), " but atlas has ", K, " parcels.")
  }
  if (length(map2) != K) {
    stop("length(map2) = ", length(map2), " but atlas has ", K, " parcels.")
  }

  if (!requireNamespace("Rnanoflann", quietly = TRUE)) {
    stop("Package 'Rnanoflann' is required for spin_test().")
  }

  if (!is.null(seed)) set.seed(seed)

  # --- 1. Obtain sphere coordinates ---
  if (is.null(sphere)) {
    sphere <- .get_atlas_sphere(atlas)
  }

  # --- 2. Compute parcel centroids on the sphere ---
  centroids <- .parcel_sphere_centroids(atlas, sphere)

  # --- 3. Observed correlation ---
  observed <- stats::cor(map1, map2, method = cor_method)

  # --- 4. Null distribution ---
  null_dist <- numeric(n_perm)

  for (i in seq_len(n_perm)) {
    R_mat <- .random_rotation_matrix()
    reassign <- .spin_reassignment(centroids, R_mat)
    null_dist[i] <- stats::cor(map1, map2[reassign], method = cor_method)
  }

  # --- 5. Two-sided p-value ---
  p_value <- (sum(abs(null_dist) >= abs(observed)) + 1L) / (n_perm + 1L)

  structure(
    list(
      observed          = observed,
      null_distribution = null_dist,
      p_value           = p_value,
      n_perm            = n_perm
    ),
    class = "spin_test"
  )
}


#' @export
print.spin_test <- function(x, ...) {
  cat("Spin Test Result\n")
  cat("  Observed r: ", round(x$observed, 4), "\n")
  cat("  Permutations: ", x$n_perm, "\n")
  cat("  p-value: ", format.pval(x$p_value, digits = 4), "\n")
  invisible(x)
}


# ---- Internal helpers --------------------------------------------------------

#' Fetch sphere coordinates for a surfatlas
#'
#' @param atlas A surfatlas object.
#' @return A list with \code{lh} and \code{rh}, each an N x 3 coordinate
#'   matrix.
#' @keywords internal
#' @noRd
.get_atlas_sphere <- function(atlas) {
  if (!inherits(atlas, "surfatlas")) {
    stop("Automatic sphere lookup requires a 'surfatlas' object. ",
         "Provide 'sphere' explicitly for other atlas types.")
  }

  # Determine the template_id and density/resolution from atlas metadata
  space <- atlas$surface_space
  if (is.null(space)) space <- "fsaverage6"
  mapping <- .schaefer_surface_space_mapping(space)

  lh <- load_surface_template(
    template_id = mapping$template_id,
    surface_type = "sphere",
    hemi = "L",
    density = mapping$tf_density,
    resolution = mapping$tf_resolution
  )
  rh <- load_surface_template(
    template_id = mapping$template_id,
    surface_type = "sphere",
    hemi = "R",
    density = mapping$tf_density,
    resolution = mapping$tf_resolution
  )

  list(lh = neurosurf::coords(lh), rh = neurosurf::coords(rh))
}


#' Compute parcel centroids on the sphere surface
#'
#' Averages the sphere coordinates of all vertices belonging to each parcel.
#'
#' @param atlas A surfatlas atlas object, or any atlas with lh_atlas/rh_atlas.
#' @param sphere A list with \code{lh} and \code{rh} N x 3 matrices.
#' @return A K x 3 matrix of centroid coordinates (one row per parcel, in
#'   \code{atlas$ids} order).
#' @keywords internal
#' @noRd
.parcel_sphere_centroids <- function(atlas, sphere) {
  K <- length(atlas$ids)
  centroids <- matrix(NA_real_, nrow = K, ncol = 3)

  # If sphere is a K x 3 matrix of parcel centroids directly, use as-is

  if (is.matrix(sphere) && nrow(sphere) == K && ncol(sphere) == 3) {
    return(sphere)
  }

  # Extract per-vertex labels for each hemisphere
  lh_labels <- if (!is.null(atlas$lh_atlas)) as.integer(atlas$lh_atlas@data) else integer(0)
  rh_labels <- if (!is.null(atlas$rh_atlas)) as.integer(atlas$rh_atlas@data) else integer(0)

  # Combine coordinates and labels
  all_coords <- rbind(sphere$lh, sphere$rh)
  all_labels <- c(lh_labels, rh_labels)

  for (i in seq_len(K)) {
    pid <- atlas$ids[i]
    mask <- all_labels == pid
    if (any(mask)) {
      centroids[i, ] <- colMeans(all_coords[mask, , drop = FALSE])
    }
  }

  centroids
}


#' Generate a uniform random rotation matrix (Haar measure on SO(3))
#'
#' Uses the QR decomposition of a random 3 x 3 Gaussian matrix, corrected
#' for sign to ensure a proper rotation (determinant +1).
#'
#' @return A 3 x 3 orthogonal rotation matrix.
#' @keywords internal
#' @noRd
.random_rotation_matrix <- function() {
  M <- matrix(stats::rnorm(9), 3, 3)
  qr_out <- qr(M)
  Q <- qr.Q(qr_out)
  R <- qr.R(qr_out)
  # Correct signs so that diagonal of R is positive -> uniform Haar measure
  d <- sign(diag(R))
  d[d == 0] <- 1
  Q <- Q %*% diag(d)
  # Ensure proper rotation (det = +1, not -1)
  if (det(Q) < 0) Q[, 1] <- -Q[, 1]
  Q
}


#' Reassign parcel centroids after a spin rotation
#'
#' Rotates centroids and finds the nearest original centroid for each
#' rotated one using \code{Rnanoflann::nn}.
#'
#' @param centroids K x 3 matrix of original centroids.
#' @param R_mat 3 x 3 rotation matrix.
#' @return Integer vector of length K giving the reassignment indices.
#' @keywords internal
#' @noRd
.spin_reassignment <- function(centroids, R_mat) {
  rotated <- centroids %*% t(R_mat)
  nn_result <- Rnanoflann::nn(data = centroids, points = rotated, k = 1L)
  as.integer(nn_result$indices)
}

#' Spin Test for Spatial Correlation Significance
#'
#' @description
#' Tests whether the spatial correlation between two parcellated brain maps is
#' stronger than expected by chance, using the spin-test framework of
#' Alexander-Bloch et al. (2018). Parcel centroids on each hemisphere's sphere
#' are randomly rotated and reassigned to the nearest original centroid of the
#' same hemisphere, generating a null distribution of correlations that
#' preserves spatial autocorrelation and hemisphere membership.
#'
#' @param map1 Numeric vector of parcel values (length K, aligned to atlas
#'   parcels).
#' @param map2 Numeric vector of parcel values (length K, aligned to atlas
#'   parcels).
#' @param atlas A surface atlas object (class \code{"surfatlas"}) with
#'   \code{lh_atlas} and \code{rh_atlas} fields, or any atlas whose geometry
#'   can provide sphere coordinates. When \code{sphere} is a K x 3 centroid
#'   matrix, \code{atlas$hemi} must give each parcel's hemisphere
#'   (\code{"left"} or \code{"right"}).
#' @param n_perm Integer. Number of spin permutations. Default: 1000.
#' @param cor_method Character. Correlation method passed to
#'   \code{\link[stats]{cor}}.
#'   One of \code{"pearson"}, \code{"spearman"}, or \code{"kendall"}.
#'   Default: \code{"pearson"}.
#' @param sphere Optional sphere geometry. Either a list with elements
#'   \code{lh} and \code{rh}, each an N x 3 matrix of sphere vertex
#'   coordinates for that hemisphere (in the hemisphere's own sphere
#'   coordinate frame), or a K x 3 matrix of parcel centroids in
#'   \code{atlas$ids} order. If \code{NULL} (default), sphere coordinates are
#'   fetched via \code{\link{load_surface_template}(..., "sphere")} using the
#'   atlas's \code{surface_space}.
#' @param seed Optional integer seed for reproducibility.
#' @param hemi_rotation Character. How the right-hemisphere rotation relates
#'   to the left-hemisphere rotation in each permutation. \code{"mirror"}
#'   (default, as in Alexander-Bloch et al. 2018) draws one random rotation
#'   for the left hemisphere and applies its reflection across the
#'   mid-sagittal (x = 0) plane to the right hemisphere, so homologous
#'   locations rotate homologously. \code{"independent"} draws a separate
#'   random rotation for each hemisphere. In both cases each hemisphere is
#'   rotated on its own sphere and parcels are only reassigned within their
#'   own hemisphere.
#'
#' @return A list of class \code{"spin_test"} with:
#' \describe{
#'   \item{observed}{The observed correlation between \code{map1} and
#' \code{map2}.}
#'   \item{null_distribution}{Numeric vector of length \code{n_perm} containing
#'     null correlations.}
#'   \item{p_value}{Two-sided p-value: proportion of null correlations with
#'     absolute value >= the observed absolute correlation.}
#'   \item{n_perm}{Number of permutations performed.}
#'   \item{hemi_rotation}{The hemisphere rotation scheme used.}
#'   \item{permutations}{K x \code{n_perm} integer matrix; column \code{i}
#'     gives, for each parcel, the index of the parcel whose \code{map2}
#'     value it receives in permutation \code{i}.}
#' }
#'
#' @details
#' The algorithm:
#' \enumerate{
#'   \item Compute parcel centroids on each hemisphere's sphere by averaging
#'     the sphere coordinates of that hemisphere's vertices belonging to each
#'     parcel.
#'   \item For each permutation, generate a uniform random 3D rotation matrix
#'     (from the Haar measure on SO(3)) for the left hemisphere and derive the
#'     right-hemisphere rotation according to \code{hemi_rotation}. Rotate the
#'     centroids of each hemisphere with that hemisphere's rotation, then
#'     reassign each rotated centroid to its nearest original centroid in the
#'     \emph{same} hemisphere using \code{\link[Rnanoflann]{nn}}.
#'   \item Compute the correlation of \code{map1} with the permuted
#'     \code{map2} (values shuffled according to the reassignment).
#'   \item The p-value is \code{(sum(abs(null) >= abs(observed)) + 1) /
#'     (n_perm + 1)}.
#' }
#'
#' Nearest-centroid reassignment can map several rotated parcels to the same
#' original parcel (sampling with replacement), as in the original
#' Alexander-Bloch et al. (2018) procedure.
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
#' res <- spin_test(map1, map2, atlas, n_perm = 500, seed = 42)
#' print(res)
#' }
#'
#' @export
spin_test <- function(
  map1,
  map2,
  atlas,
  n_perm = 1000L,
  cor_method = c("pearson", "spearman", "kendall"),
  sphere = NULL,
  seed = NULL,
  hemi_rotation = c("mirror", "independent")
) {
  cor_method <- match.arg(cor_method)
  hemi_rotation <- match.arg(hemi_rotation)
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

  # --- 2. Compute per-hemisphere parcel centroids on the sphere ---
  centroids <- .parcel_sphere_centroids(atlas, sphere)
  hemi <- attr(centroids, "hemi")

  # --- 3. Observed correlation ---
  observed <- stats::cor(map1, map2, method = cor_method)

  # --- 4. Null distribution ---
  null_dist <- numeric(n_perm)
  perms <- matrix(NA_integer_, nrow = K, ncol = n_perm)

  for (i in seq_len(n_perm)) {
    rotations <- .spin_hemi_rotations(hemi_rotation)
    reassign <- .spin_reassignment(centroids, rotations, hemi)
    perms[, i] <- reassign
    null_dist[i] <- stats::cor(map1, map2[reassign], method = cor_method)
  }

  # --- 5. Two-sided p-value ---
  p_value <- (sum(abs(null_dist) >= abs(observed)) + 1L) / (n_perm + 1L)

  structure(
    list(
      observed          = observed,
      null_distribution = null_dist,
      p_value           = p_value,
      n_perm            = n_perm,
      hemi_rotation     = hemi_rotation,
      permutations      = perms
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
    stop(
      "Automatic sphere lookup requires a 'surfatlas' object. ",
      "Provide 'sphere' explicitly for other atlas types."
    )
  }

  space <- atlas$surface_space
  if (is.null(space)) space <- "fsaverage6"
  mapping <- .schaefer_surface_space_mapping(space)

  # TemplateFlow indexes fsaverage surfaces by density (den-41k, den-10k,
  # den-164k); fsaverage has no surface `res-` entity, so only density is
  # passed (#55).
  load_sphere <- function(h) {
    load_surface_template(
      template_id = mapping$template_id,
      surface_type = "sphere",
      hemi = h,
      density = mapping$tf_density
    )
  }

  list(
    lh = neurosurf::coords(load_sphere("L")),
    rh = neurosurf::coords(load_sphere("R"))
  )
}


#' Normalise hemisphere labels to "left"/"right"
#' @keywords internal
#' @noRd
.spin_normalize_hemi <- function(hemi) {
  h <- tolower(as.character(hemi))
  out <- rep(NA_character_, length(h))
  out[h %in% c("left", "lh", "l")] <- "left"
  out[h %in% c("right", "rh", "r")] <- "right"
  out
}


#' Compute parcel centroids on each hemisphere's sphere surface
#'
#' Averages the sphere coordinates of the vertices belonging to each parcel,
#' using only the vertices of the hemisphere that contains the parcel.
#'
#' @param atlas A surfatlas atlas object, or any atlas with lh_atlas/rh_atlas.
#' @param sphere A list with \code{lh} and \code{rh} N x 3 matrices, or a
#'   K x 3 matrix of parcel centroids.
#' @return A K x 3 matrix of centroid coordinates (one row per parcel, in
#'   \code{atlas$ids} order) with attribute \code{"hemi"}: a character vector
#'   of \code{"left"}/\code{"right"} giving each parcel's hemisphere.
#' @keywords internal
#' @noRd
.parcel_sphere_centroids <- function(atlas, sphere) {
  K <- length(atlas$ids)

  # A K x 3 matrix of parcel centroids: hemisphere comes from atlas$hemi.
  if (is.matrix(sphere)) {
    if (nrow(sphere) != K || ncol(sphere) != 3) {
      stop(
        "A centroid matrix 'sphere' must be K x 3 (K = ", K, " parcels)."
      )
    }
    hemi <- if (is.null(atlas$hemi)) {
      rep(NA_character_, K)
    } else {
      .spin_normalize_hemi(atlas$hemi)
    }
    if (length(hemi) != K || anyNA(hemi)) {
      stop(
        "spin_test() rotates each hemisphere separately, so a K x 3 ",
        "centroid matrix requires 'atlas$hemi' to label every parcel ",
        "'left' or 'right'."
      )
    }
    centroids <- unname(as.matrix(sphere))
    storage.mode(centroids) <- "double"
    attr(centroids, "hemi") <- hemi
    return(centroids)
  }

  if (!is.list(sphere) || is.null(sphere$lh) && is.null(sphere$rh)) {
    stop(
      "'sphere' must be a list with 'lh' and 'rh' N x 3 coordinate ",
      "matrices, or a K x 3 matrix of parcel centroids."
    )
  }

  label_vec <- function(x) {
    if (is.null(x)) integer(0) else as.integer(x@data)
  }
  hemi_labels <- list(
    left = label_vec(atlas$lh_atlas),
    right = label_vec(atlas$rh_atlas)
  )
  hemi_coords <- list(left = sphere$lh, right = sphere$rh)
  for (h in names(hemi_labels)) {
    n_lab <- length(hemi_labels[[h]])
    if (n_lab > 0 && (is.null(hemi_coords[[h]]) ||
      nrow(hemi_coords[[h]]) != n_lab)) {
      stop(
        "Sphere for the ", h, " hemisphere has ",
        if (is.null(hemi_coords[[h]])) 0L else nrow(hemi_coords[[h]]),
        " vertices but the atlas labels ", n_lab, " vertices."
      )
    }
  }

  centroids <- matrix(NA_real_, nrow = K, ncol = 3)
  hemi <- rep(NA_character_, K)

  for (i in seq_len(K)) {
    pid <- atlas$ids[i]
    in_hemi <- vapply(
      hemi_labels, function(lab) any(lab == pid), logical(1)
    )
    if (all(in_hemi)) {
      stop(
        "Parcel id ", pid, " has vertices in both hemispheres; spin_test() ",
        "requires each parcel to belong to a single hemisphere."
      )
    }
    if (!any(in_hemi)) next
    h <- names(hemi_labels)[in_hemi]
    mask <- hemi_labels[[h]] == pid
    centroids[i, ] <- colMeans(hemi_coords[[h]][mask, , drop = FALSE])
    hemi[i] <- h
  }

  missing <- is.na(hemi)
  if (any(missing)) {
    stop(
      "No sphere vertices found for parcel id(s): ",
      paste(utils::head(atlas$ids[missing], 10), collapse = ", "),
      if (sum(missing) > 10) ", ..." else "",
      "."
    )
  }

  attr(centroids, "hemi") <- hemi
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


#' Draw one pair of hemisphere rotations
#'
#' @param hemi_rotation \code{"mirror"}: the right-hemisphere rotation is the
#'   left rotation reflected across the x = 0 plane, \code{F R F} with
#'   \code{F = diag(c(-1, 1, 1))} (Alexander-Bloch et al. 2018).
#'   \code{"independent"}: a separate random rotation per hemisphere.
#' @return A list with 3 x 3 rotation matrices \code{left} and \code{right}.
#' @keywords internal
#' @noRd
.spin_hemi_rotations <- function(hemi_rotation = c("mirror", "independent")) {
  hemi_rotation <- match.arg(hemi_rotation)
  left <- .random_rotation_matrix()
  right <- if (identical(hemi_rotation, "mirror")) {
    reflect <- diag(c(-1, 1, 1))
    reflect %*% left %*% reflect
  } else {
    .random_rotation_matrix()
  }
  list(left = left, right = right)
}


#' Reassign parcel centroids after a spin rotation, within hemispheres
#'
#' Rotates each hemisphere's centroids with that hemisphere's rotation and
#' finds the nearest original centroid of the same hemisphere for each
#' rotated one using \code{Rnanoflann::nn}.
#'
#' @param centroids K x 3 matrix of original centroids.
#' @param rotations List with 3 x 3 rotation matrices \code{left} and
#'   \code{right}, as returned by \code{.spin_hemi_rotations()}.
#' @param hemi Character vector of length K (\code{"left"}/\code{"right"}).
#' @return Integer vector of length K giving the reassignment indices into
#'   \code{seq_len(K)}; every index points to a parcel of the same hemisphere.
#' @keywords internal
#' @noRd
.spin_reassignment <- function(centroids, rotations, hemi) {
  K <- nrow(centroids)
  out <- integer(K)
  for (h in c("left", "right")) {
    idx <- which(hemi == h)
    if (length(idx) == 0L) next
    base <- centroids[idx, , drop = FALSE]
    if (length(idx) == 1L) {
      out[idx] <- idx
      next
    }
    rotated <- base %*% t(rotations[[h]])
    nn_result <- Rnanoflann::nn(data = base, points = rotated, k = 1L)
    out[idx] <- idx[as.integer(nn_result$indices)]
  }
  out
}

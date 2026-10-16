# Spin permutation tests.

# Build a synthetic sphere + surfatlas for testing without TemplateFlow.
# Creates two hemispheres with 5 parcels each on a unit sphere.
make_spin_atlas <- function() {
  # Generate sphere vertices: regular grid on unit sphere via spherical coords
  n_vert <- 200 # per hemisphere
  set.seed(123)
  theta <- stats::runif(n_vert, 0, pi)
  phi <- stats::runif(n_vert, 0, 2 * pi)
  lh_coords <- cbind(
    sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )
  rh_coords <- cbind(
    -sin(theta) * cos(phi),
    # mirror x for right hemi
    sin(theta) * sin(phi),
    cos(theta)
  )

  # Assign parcels based on z-coordinate bins (5 parcels per hemi)
  z_breaks <- seq(-1, 1, length.out = 6)
  lh_labels <- as.integer(cut(lh_coords[, 3], z_breaks, include.lowest = TRUE))
  rh_labels <- as.integer(
    cut(
      rh_coords[, 3],
      z_breaks,
      include.lowest = TRUE
    )
  ) + 5L

  # Build minimal SurfaceGeometry + labeled surface-like objects
  # We use a simple structure that has @data for labels
  lh_atlas_obj <- structure(list(), class = "mock_surf")
  lh_atlas_obj@data <- lh_labels
  rh_atlas_obj <- structure(list(), class = "mock_surf")
  rh_atlas_obj@data <- rh_labels

  # Use setattr approach instead since mock objects need @data slot
  lh_atlas_obj <- new("mock_labeled_surf", data = lh_labels)
  rh_atlas_obj <- new("mock_labeled_surf", data = rh_labels)

  atlas <- structure(
    list(
      name = "spin_test_atlas",
      ids = 1:10,
      labels = paste0("R", 1:10),
      orig_labels = paste0("Region_", 1:10),
      hemi = rep(c("left", "right"), each = 5),
      lh_atlas = lh_atlas_obj,
      rh_atlas = rh_atlas_obj,
      surface_space = "fsaverage6"
    ),
    class = c("test_surfatlas", "surfatlas", "atlas")
  )

  sphere <- list(lh = lh_coords, rh = rh_coords)

  list(atlas = atlas, sphere = sphere)
}

# We need a simple S4 class with a @data slot for the mock
# Define it outside of tests so it's available
if (!isClass("mock_labeled_surf")) {
  setClass("mock_labeled_surf", representation(data = "integer"))
}

make_spin_atlas <- function() {
  n_vert <- 200
  set.seed(123)
  theta <- stats::runif(n_vert, 0, pi)
  phi <- stats::runif(n_vert, 0, 2 * pi)
  lh_coords <- cbind(
    sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )
  rh_coords <- cbind(
    -sin(theta) * cos(phi),
    sin(theta) * sin(phi),
    cos(theta)
  )

  z_breaks <- seq(-1, 1, length.out = 6)
  lh_labels <- as.integer(cut(lh_coords[, 3], z_breaks, include.lowest = TRUE))
  rh_labels <- as.integer(
    cut(
      rh_coords[, 3],
      z_breaks,
      include.lowest = TRUE
    )
  ) + 5L

  lh_atlas_obj <- new("mock_labeled_surf", data = lh_labels)
  rh_atlas_obj <- new("mock_labeled_surf", data = rh_labels)

  atlas <- structure(
    list(
      name = "spin_test_atlas",
      ids = 1:10,
      labels = paste0("R", 1:10),
      orig_labels = paste0("Region_", 1:10),
      hemi = rep(c("left", "right"), each = 5),
      lh_atlas = lh_atlas_obj,
      rh_atlas = rh_atlas_obj,
      surface_space = "fsaverage6"
    ),
    class = c("test_surfatlas", "surfatlas", "atlas")
  )

  sphere <- list(lh = lh_coords, rh = rh_coords)

  list(atlas = atlas, sphere = sphere)
}


test_that(
  "spin_test returns correct structure",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)

    res <- spin_test(
      map1 = rnorm(K),
      map2 = rnorm(K),
      atlas = sa$atlas,
      sphere = sa$sphere,
      n_perm = 50,
      seed = 1
    )

    expect_s3_class(res, "spin_test")
    expect_true(is.numeric(res$observed))
    expect_length(res$null_distribution, 50L)
    expect_true(res$p_value >= 0 && res$p_value <= 1)
    expect_equal(res$n_perm, 50L)
  }
)


test_that(
  "identical maps produce low p-value",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)

    set.seed(42)
    map <- rnorm(K)

    res <- spin_test(
      map1 = map,
      map2 = map,
      atlas = sa$atlas,
      sphere = sa$sphere,
      n_perm = 199,
      seed = 99
    )

    # With identical maps, observed r = 1.0
    expect_equal(res$observed, 1.0)
    # p-value should be small (most null correlations < 1.0)
    expect_true(res$p_value < 0.2)
  }
)


test_that(
  "uncorrelated maps produce non-significant p-value",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)

    set.seed(7)
    map1 <- rnorm(K)
    map2 <- rnorm(K)

    res <- spin_test(
      map1 = map1,
      map2 = map2,
      atlas = sa$atlas,
      sphere = sa$sphere,
      n_perm = 199,
      seed = 7
    )

    # With random maps, p should generally be non-significant
    # Use a generous threshold since K=10 is small
    expect_true(res$p_value > 0.01)
  }
)


test_that(
  "seed produces reproducible results",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)
    m1 <- seq_len(K) * 1.0
    m2 <- rev(seq_len(K)) * 1.0

    r1 <- spin_test(
      m1, m2, sa$atlas, sphere = sa$sphere, n_perm = 50, seed = 42
    )
    r2 <- spin_test(
      m1, m2, sa$atlas, sphere = sa$sphere, n_perm = 50, seed = 42
    )

    expect_equal(r1$null_distribution, r2$null_distribution)
    expect_equal(r1$p_value, r2$p_value)
  }
)


test_that(
  "input validation catches wrong lengths",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()

    expect_error(
      spin_test(1:5, 1:10, sa$atlas, sphere = sa$sphere, n_perm = 10),
      "length\\(map1\\)"
    )
    expect_error(
      spin_test(1:10, 1:5, sa$atlas, sphere = sa$sphere, n_perm = 10),
      "length\\(map2\\)"
    )
  }
)


test_that(
  ".random_rotation_matrix returns proper rotation",
  {
    set.seed(1)
    Q <- neuroatlas:::.random_rotation_matrix()

    expect_equal(dim(Q), c(3L, 3L))
    # Orthogonal: Q^T Q = I
    expect_equal(t(Q) %*% Q, diag(3), tolerance = 1e-10)
    # Proper rotation: det = +1
    expect_equal(det(Q), 1.0, tolerance = 1e-10)
  }
)


test_that(
  "print.spin_test works",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)

    res <- spin_test(
      rnorm(K),
      rnorm(K),
      sa$atlas,
      sphere = sa$sphere,
      n_perm = 10,
      seed = 1
    )

    out <- capture.output(print(res))
    expect_true(any(grepl("Spin Test", out)))
    expect_true(any(grepl("Observed r", out)))
    expect_true(any(grepl("p-value", out)))
  }
)


# ---- Regression tests: hemisphere-preserving spins (#56) --------------------

test_that(
  "no spin permutation reassigns a parcel to the opposite hemisphere (#56)",
  {
    skip_if_not_installed("Rnanoflann")
    # The mock lh and rh spheres overlap (rh is lh mirrored in x, both centred
    # on the origin), so pooled nearest-neighbour matching would cross
    # hemispheres in most rotations.
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)
    hemi <- sa$atlas$hemi

    for (scheme in c("mirror", "independent")) {
      res <- spin_test(
        rnorm(K), rnorm(K), sa$atlas,
        sphere = sa$sphere, n_perm = 300, seed = 11,
        hemi_rotation = scheme
      )
      expect_equal(dim(res$permutations), c(K, 300L))
      expect_identical(res$hemi_rotation, scheme)
      cross <- apply(res$permutations, 2, function(p) any(hemi[p] != hemi))
      expect_false(any(cross), info = scheme)
      # Reassignments are not all trivial identities
      expect_true(
        any(apply(res$permutations, 2, function(p) any(p != seq_len(K))))
      )
    }
  }
)

test_that(
  "user-supplied centroid matrix spins within atlas$hemi (#56)",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    K <- length(sa$atlas$ids)
    cent <- neuroatlas:::.parcel_sphere_centroids(sa$atlas, sa$sphere)
    cent_plain <- matrix(as.numeric(cent), ncol = 3)

    res <- spin_test(
      rnorm(K), rnorm(K), sa$atlas,
      sphere = cent_plain, n_perm = 200, seed = 3
    )
    hemi <- sa$atlas$hemi
    expect_false(any(apply(res$permutations, 2, function(p) any(hemi[p] != hemi))))

    no_hemi <- sa$atlas
    no_hemi$hemi <- NULL
    expect_error(
      spin_test(rnorm(K), rnorm(K), no_hemi, sphere = cent_plain, n_perm = 5),
      "atlas\\$hemi"
    )
  }
)

test_that(
  "centroids are computed per hemisphere and carry hemisphere labels (#56)",
  {
    sa <- make_spin_atlas()
    cent <- neuroatlas:::.parcel_sphere_centroids(sa$atlas, sa$sphere)
    expect_identical(attr(cent, "hemi"), sa$atlas$hemi)
    lh_lab <- sa$atlas$lh_atlas@data
    expect_equal(
      as.numeric(cent[1, ]),
      as.numeric(colMeans(sa$sphere$lh[lh_lab == 1L, , drop = FALSE]))
    )
    rh_lab <- sa$atlas$rh_atlas@data
    expect_equal(
      as.numeric(cent[6, ]),
      as.numeric(colMeans(sa$sphere$rh[rh_lab == 6L, , drop = FALSE]))
    )
  }
)

test_that(
  "hemisphere rotations: mirror is the x-reflection, independent is separate (#56)",
  {
    reflect <- diag(c(-1, 1, 1))

    set.seed(5)
    mir <- neuroatlas:::.spin_hemi_rotations("mirror")
    expect_equal(mir$right, reflect %*% mir$left %*% reflect, tolerance = 1e-12)
    expect_equal(det(mir$right), 1, tolerance = 1e-10)

    # Same seed: the left rotation is the same draw in both schemes, but the
    # independent right rotation is a fresh draw, not derived from the left.
    set.seed(5)
    ind <- neuroatlas:::.spin_hemi_rotations("independent")
    expect_equal(ind$left, mir$left)
    expect_false(isTRUE(all.equal(ind$right, mir$right)))
    expect_equal(t(ind$right) %*% ind$right, diag(3), tolerance = 1e-10)
    expect_equal(det(ind$right), 1, tolerance = 1e-10)

    # Across many draws the independent right rotation is uncorrelated with
    # the left one; the mirrored one is a deterministic function of it.
    set.seed(9)
    draws <- replicate(2000, {
      r <- neuroatlas:::.spin_hemi_rotations("independent")
      c(r$left[2, 2], r$right[2, 2])
    })
    expect_lt(abs(stats::cor(draws[1, ], draws[2, ])), 0.1)
    set.seed(9)
    draws_m <- replicate(200, {
      r <- neuroatlas:::.spin_hemi_rotations("mirror")
      c(r$left[2, 2], r$right[2, 2])
    })
    expect_equal(draws_m[1, ], draws_m[2, ])
  }
)

test_that(
  "each hemisphere's reassignment depends only on its own centroids (#56)",
  {
    skip_if_not_installed("Rnanoflann")
    sa <- make_spin_atlas()
    cent <- neuroatlas:::.parcel_sphere_centroids(sa$atlas, sa$sphere)
    hemi <- attr(cent, "hemi")
    lh <- hemi == "left"

    set.seed(21)
    rot <- neuroatlas:::.spin_hemi_rotations("independent")
    base <- neuroatlas:::.spin_reassignment(cent, rot, hemi)

    # Move the right-hemisphere centroids onto the left ones: with pooled
    # matching this would change left-hemisphere assignments.
    cent2 <- cent
    cent2[!lh, ] <- cent[lh, ] + 1e-6
    alt <- neuroatlas:::.spin_reassignment(cent2, rot, hemi)
    expect_identical(alt[lh], base[lh])

    # Changing only the right rotation leaves the left assignment unchanged.
    rot2 <- rot
    rot2$right <- diag(3)
    alt2 <- neuroatlas:::.spin_reassignment(cent, rot2, hemi)
    expect_identical(alt2[lh], base[lh])
    expect_identical(alt2[!lh], which(!lh))
  }
)

test_that(
  "real Schaefer fsaverage6 spins never cross hemispheres (#55, #56)",
  {
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("Rnanoflann")
    at <- tryCatch(
      get_schaefer_surfatlas(parcels = "100", networks = "7"),
      error = function(e) skip(paste("Schaefer atlas unavailable:", conditionMessage(e)))
    )
    expect_identical(at$surface_space, "fsaverage6")
    K <- length(at$ids)
    set.seed(1)
    m1 <- rnorm(K)
    m2 <- m1 + rnorm(K)
    # Automatic sphere lookup (#55)
    res <- spin_test(m1, m2, at, n_perm = 100, seed = 1)
    expect_s3_class(res, "spin_test")
    hemi <- at$hemi
    expect_false(any(apply(res$permutations, 2, function(p) any(hemi[p] != hemi))))
  }
)


# ---- Regression tests: automatic sphere lookup (#55) ------------------------

test_that(
  "automatic sphere lookup queries fsaverage spheres by density only (#55)",
  {
    calls <- list()
    tiny <- neurosurf::SurfaceGeometry(diag(3), matrix(c(0L, 1L, 2L), 1), "lh")
    local_mocked_bindings(
      load_surface_template = function(template_id, surface_type, hemi,
                                       density = NULL, resolution = NULL,
                                       ...) {
        # Mirror TemplateFlow: fsaverage surfaces have no `res-` entity.
        if (!is.null(resolution)) {
          stop("Resolution '", resolution, "' not available for fsaverage.")
        }
        calls[[length(calls) + 1L]] <<- list(
          template_id = template_id, surface_type = surface_type,
          hemi = hemi, density = density
        )
        tiny
      }
    )

    expected <- c(fsaverage6 = "41k", fsaverage5 = "10k", fsaverage = "164k")
    for (space in names(expected)) {
      calls <- list()
      atlas <- structure(
        list(ids = 1:2, surface_space = space),
        class = c("surfatlas", "atlas")
      )
      sph <- neuroatlas:::.get_atlas_sphere(atlas)
      expect_named(sph, c("lh", "rh"))
      expect_equal(nrow(sph$lh), 3L)
      expect_length(calls, 2L)
      expect_equal(vapply(calls, `[[`, "", "density"), rep(expected[[space]], 2),
        info = space
      )
      expect_equal(vapply(calls, `[[`, "", "hemi"), c("L", "R"))
      expect_equal(vapply(calls, `[[`, "", "surface_type"), rep("sphere", 2))
    }
  }
)

test_that(
  "Schaefer surface space mapping carries no TemplateFlow resolution (#55)",
  {
    for (space in c("fsaverage", "fsaverage5", "fsaverage6")) {
      m <- neuroatlas:::.schaefer_surface_space_mapping(space)
      expect_null(m$tf_resolution, info = space)
    }
  }
)

make_toy_projection <- function(
  coords,
  cortex = rep(TRUE, 6),
  pial = coords,
  n_samples = 3L
) {
  sphere <- rbind(
    c(1, 0, 0),
    c(-1, 0, 0),
    c(0, 1, 0),
    c(0, -1, 0),
    c(0, 0, 1),
    c(0, 0, -1)
  )
  faces <- rbind(
    c(0, 2, 4),
    c(2, 1, 4),
    c(1, 3, 4),
    c(3, 0, 4),
    c(2, 0, 5),
    c(1, 2, 5),
    c(3, 1, 5),
    c(0, 3, 5)
  )
  domain <- surface_domain(
    "toy",
    "L",
    "6v",
    sphere,
    faces,
    cortex,
    "analytic",
    "projection-test"
  )
  ribbon_projection(domain, coords, pial, cortex, "toy-RAS", n_samples)
}

make_toy_projection_ramp <- function() {
  indices <- arrayInd(seq_len(64), c(4L, 4L, 4L)) - 1L
  array(indices[, 1] + 2 * indices[, 2] + 4 * indices[, 3], c(4L, 4L, 4L))
}

test_that("diagnostics separate map loss, absent keys and hemisphere declarations", {
  skip_if_not_installed("neurotransform", "0.2.0")
  coords <- matrix(rep(c(0, 0, 0), 6), ncol = 3, byrow = TRUE)
  values <- array(0, c(4, 4, 4, 2))
  values[2, 1, 1, 1] <- 9
  values[1, 1, 1, 2] <- 9
  values[3, 1, 1, ] <- 17
  table <- data.frame(
    key = c(0, 9, 17, 99), name = c("zero", "nine", "right", "absent"),
    hemisphere = c(NA, "left", "RH", "L")
  )
  result <- apply_surface_projection(
    values, make_toy_projection(coords), "toy-RAS", diag(4), "label",
    label_table = table
  )
  qa <- projection_diagnostics(result)
  expect_equal(qa$summary$not_sampled_labels, c(1, 0))
  expect_equal(qa$summary$lost_resampling_labels, c(0, 0))
  expect_equal(qa$summary$target_available_vertices, c(6, 6))
  nine <- subset(qa$labels, key == 9)
  expect_equal(nine$source_voxels, c(1, 1))
  expect_equal(nine$sampled_vertices, c(0, 6))
  expect_equal(nine$target_vertices, c(0, 6))
  expect_equal(nine$status, c("not_sampled", "represented"))
  expect_true(all(subset(qa$labels, key == 99)$status == "absent_source"))
  expect_true(all(subset(qa$labels, key == 17)$status == "other_hemisphere"))
  expect_false(any(subset(qa$labels, key == 17)$lost_at_sampling))
  expect_false(any(subset(qa$labels, key == 0)$lost_at_sampling))
  expect_identical(result$label_table, table)
  # Overall-key provenance remains available; map 1 loss is now explicit.
  expect_false(9 %in% result$provenance$lost_label_keys)
  result$values[1, 1] <- 99
  expect_error(projection_diagnostics(result), "unmodified")
})

test_that("diagnostics expose hemisphere leakage without changing supported values", {
  skip_if_not_installed("neurotransform", "0.2.0")
  coords <- matrix(rep(c(0, 0, 0), 6), ncol = 3, byrow = TRUE)
  table <- data.frame(key = 7, name = "custom", hemisphere = "R")
  result <- apply_surface_projection(
    array(7, c(4, 4, 4)), make_toy_projection(coords), "toy-RAS", diag(4),
    "label", label_table = table
  )
  expect_equal(result$values, rep(7, 6))
  qa <- projection_diagnostics(result)
  expect_equal(qa$summary$hemisphere_mismatch_vertices, 6)
  expect_true(qa$labels$hemisphere_mismatch)
  expect_equal(qa$summary$not_sampled_labels, 0)
  table$hemisphere <- NULL
  table$name <- "RH_this_name_does_not_declare_a_hemisphere"
  unchecked <- apply_surface_projection(
    array(7, c(4, 4, 4)), make_toy_projection(coords), "toy-RAS", diag(4),
    "label", label_table = table
  )
  expect_equal(projection_diagnostics(unchecked)$summary$unchecked_labels, 1)
  expect_equal(
    projection_diagnostics(unchecked)$summary$hemisphere_mismatch_vertices, 0
  )
  table$hemisphere <- "infer_from_name"
  expect_error(apply_surface_projection(
    array(7, c(4, 4, 4)), make_toy_projection(coords), "toy-RAS", diag(4),
    "label", label_table = table
  ), "hemisphere")
})

test_that("diagnostics localize categorical voting loss in a composed surface stage", {
  skip_if_not_installed("neurotransform", "0.2.0")
  coords <- rbind(c(0, 0, 0), c(1, 0, 0), matrix(0, 4, 3))
  projection <- make_toy_projection(coords)
  # Rotate the target sphere so each target receives a vote from three sources.
  sphere <- rbind(
    c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0),
    c(0, 0, 1), c(0, 0, -1)
  )
  faces <- rbind(
    c(0, 2, 4), c(2, 1, 4), c(1, 3, 4), c(3, 0, 4),
    c(2, 0, 5), c(1, 2, 5), c(3, 1, 5), c(0, 3, 5)
  )
  base <- surface_geometry(
    projection$specification$target, sphere, faces, rep(TRUE, 6)
  )
  rotation <- rbind(
    c(1, 1, 1) / sqrt(3), c(1, -1, 0) / sqrt(2), c(1, 1, -2) / sqrt(6)
  )
  target_sphere <- sphere %*% rotation
  mask <- c(FALSE, rep(TRUE, 5))
  target_domain <- surface_domain(
    "toy", "L", "6v", target_sphere, faces, mask, "analytic", "projection-test"
  )
  target <- surface_geometry(target_domain, target_sphere, faces, mask)
  projection$specification$target <- target$domain
  projection$surface_operator <- get_surface_transform(base, target, NULL)
  projection$id <- neuroatlas:::.surface_projection_id(projection)
  values <- array(4, c(4, 4, 4))
  values[2, 1, 1] <- 9
  result <- apply_surface_projection(
    values, projection, "toy-RAS", diag(4), "label",
    label_table = data.frame(key = c(4, 9), hemisphere = "L")
  )
  qa <- projection_diagnostics(result)
  nine <- subset(qa$labels, key == 9)
  expect_equal(nine$source_voxels, 1)
  expect_equal(nine$sampled_vertices, 1)
  expect_equal(nine$target_vertices, 0)
  expect_identical(nine$status, "lost_resampling")
  expect_equal(qa$summary$lost_resampling_labels, 1)
  expect_equal(qa$summary$target_cortex_vertices, 5)
  expect_equal(qa$summary$target_available_vertices, 5)
  expect_true(is.na(result$values[1]))
})

test_that("noncategorical and missing-only diagnostics keep coverage distinct", {
  skip_if_not_installed("neurotransform", "0.2.0")
  coords <- matrix(rep(c(0, 0, 0), 6), ncol = 3, byrow = TRUE)
  projection <- make_toy_projection(coords, c(rep(TRUE, 4), rep(FALSE, 2)))
  result <- apply_surface_projection(
    array(NA_real_, c(4, 4, 4)), projection, "toy-RAS", diag(4), "continuous"
  )
  qa <- projection_diagnostics(result)
  expect_null(qa$labels)
  expect_equal(qa$summary$sampled_cortex_vertices, 4)
  expect_equal(qa$summary$sampled_available_vertices, 0)
  expect_equal(qa$summary$source_finite_voxels, 0)
  result <- apply_surface_projection(
    array(NA_real_, c(4, 4, 4)), projection, "toy-RAS", diag(4), "label"
  )
  expect_equal(nrow(projection_diagnostics(result)$labels), 0)
  expect_equal(projection_diagnostics(result)$summary$not_sampled_labels, 0)
  unprojected <- surface_data(rep(1, 6), projection$specification$target)
  expect_error(projection_diagnostics(unprojected), "diagnostics")
})

test_that(
  "projection preserves ramps, valid zero, bounds and cortical masks",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- rbind(
      c(0, 0, 0),
      c(1.5, 1, 1),
      c(3, 3, 3),
      c(-1e-8, 1, 1),
      c(1, 1, 1),
      c(1, 1, 1)
    )
    projection <- make_toy_projection(coords, c(rep(TRUE, 5), FALSE))
    ramp <- make_toy_projection_ramp()
    result <- apply_surface_projection(
      ramp,
      projection,
      "toy-RAS",
      diag(4),
      "continuous"
    )
    expect_equal(result$values, c(0, 7.5, 21, NA, 7, NA))
    expect_equal(
      result$coverage$available[, 1],
      c(
        TRUE,
        TRUE,
        TRUE,
        FALSE,
        TRUE,
        FALSE
      )
    )
    expect_equal(result$coverage$geometric_fraction, c(1, 1, 1, 0, 1, 0))
    expect_equal(
      result$coverage$status[, 1],
      c(
        "available",
        "available",
        "available",
        "outside",
        "available",
        "target_masked"
      )
    )
    expect_error(
      apply_surface_projection(
        ramp,
        projection,
        "MNI152",
        diag(4),
        "continuous"
      ),
      "exact source frame"
    )
    broken <- projection
    broken$points[[1]][1, 1] <- 1
    expect_error(
      apply_surface_projection(
        ramp,
        broken,
        "toy-RAS",
        diag(4),
        "continuous"
      ),
      "modified"
    )
  }
)

test_that(
  "voxel-to-world conversion works with an oblique affine",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    affine <- rbind(
      c(0, -2, 0.1, 20),
      c(1.5, 0, 0.2, -30),
      c(0, 0.1, 3, 7),
      c(0, 0, 0, 1)
    )
    voxel <- matrix(rep(c(1.25, 1.75, 0.5), 6), ncol = 3, byrow = TRUE)
    coords <- (cbind(voxel, 1) %*% t(affine))[, 1:3]
    result <- apply_surface_projection(
      make_toy_projection_ramp(),
      make_toy_projection(coords),
      "toy-RAS",
      affine,
      "continuous"
    )
    expect_equal(result$values, rep(6.75, 6), tolerance = 1e-12)
    affine[4, 1] <- 1
    expect_error(
      apply_surface_projection(
        make_toy_projection_ramp(),
        make_toy_projection(coords),
        "toy-RAS",
        affine,
        "continuous"
      ),
      "affine"
    )
  }
)

test_that(
  "missingness follows positive contributors including tiny weights",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- matrix(rep(c(1e-8, 0, 0), 6), ncol = 3, byrow = TRUE)
    projection <- make_toy_projection(coords)
    values <- array(NA_real_, c(4, 4, 4))
    values[2, 1, 1] <- 17
    propagate <- apply_surface_projection(
      values,
      projection,
      "toy-RAS",
      diag(4),
      "continuous"
    )
    expect_true(all(is.na(propagate$values)))
    omit <- apply_surface_projection(
      values,
      projection,
      "toy-RAS",
      diag(4),
      "continuous",
      "omit"
    )
    expect_equal(omit$values, rep(17, 6), tolerance = 1e-12)
    expect_equal(
      omit$coverage$source_weight_mass[, 1],
      rep(1e-8, 6),
      tolerance = 1e-15
    )
    expect_error(
      apply_surface_projection(
        values,
        projection,
        "toy-RAS",
        diag(4),
        "continuous",
        "error"
      ),
      "positive contributor"
    )
    # At an exact voxel center all adjacent missing voxels have zero weight.
    coords[, ] <- 0
    values[1, 1, 1] <- 0
    result <- apply_surface_projection(
      values,
      make_toy_projection(coords),
      "toy-RAS",
      diag(4),
      "continuous",
      "error"
    )
    expect_equal(result$values, rep(0, 6))
  }
)

test_that(
  "nearest labels use lower voxel ties and ribbon votes preserve key zero",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- matrix(rep(c(0.5, 0, 0), 6), ncol = 3, byrow = TRUE)
    values <- array(0, c(4, 4, 4))
    values[2, 1, 1] <- 9
    table <- data.frame(key = c(0, 9), name = c("zero", "nine"))
    result <- apply_surface_projection(
      values,
      make_toy_projection(coords),
      "toy-RAS",
      diag(4),
      "label",
      label_table = table
    )
    expect_equal(result$values, rep(0, 6))
    expect_identical(result$label_table, table)
    white <- coords
    white[, 1] <- 0
    pial <- white
    pial[, 1] <- 1
    result <- apply_surface_projection(
      values,
      make_toy_projection(white, pial = pial, n_samples = 2L),
      "toy-RAS",
      diag(4),
      "label",
      label_table = table
    )
    expect_equal(result$values, rep(0, 6))
    expect_equal(result$provenance$lost_label_keys, 9)
    expect_error(
      apply_surface_projection(
        values,
        make_toy_projection(coords),
        "toy-RAS",
        diag(4),
        "label",
        label_table = table[1, ]
      ),
      "every input label"
    )
  }
)

test_that(
  "ribbon quadrature and probability maps retain partial mass",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    white <- matrix(rep(c(0, 1, 1), 6), ncol = 3, byrow = TRUE)
    pial <- white
    pial[, 1] <- 2
    ramp <- make_toy_projection_ramp()
    for (n in c(3L, 5L, 9L)) {
      projection <- make_toy_projection(white, pial = pial, n_samples = n)
      result <- apply_surface_projection(
        ramp,
        projection,
        "toy-RAS",
        diag(4),
        "continuous"
      )
      expect_equal(result$values, rep(7, 6), tolerance = 1e-12)
      probability <- array(c(rep(0.25, 64), rep(0.5, 64)), c(4, 4, 4, 2))
      probability[1, 2, 2, 1] <- NA
      result <- apply_surface_projection(
        probability,
        projection,
        "toy-RAS",
        diag(4),
        "probability",
        "omit"
      )
      expect_equal(
        result$values,
        cbind(rep(0.25, 6), rep(0.5, 6)),
        tolerance = 1e-12
      )
      expect_equal(rowSums(result$values), rep(0.75, 6), tolerance = 1e-12)
    }
  }
)

test_that(
  "neuroimaging objects use their affine and metadata is checked",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- matrix(rep(c(1, 1, 1), 6), ncol = 3, byrow = TRUE)
    projection <- make_toy_projection(coords)
    grid <- neuroim2::NeuroSpace(c(4, 4, 4), trans = diag(4))
    volume <- neuroim2::NeuroVol(make_toy_projection_ramp(), grid)
    result <- apply_template_transform(volume, projection, data_type = "continuous")
    expect_equal(result$values, rep(7, 6))
    expect_error(
      apply_surface_projection(
        volume,
        projection,
        "toy-RAS",
        diag(4),
        "continuous"
      ),
      "own affine"
    )
    expect_error(apply_surface_projection(volume, projection, "toy-RAS"), "data_type")
  }
)

test_that(
  "atlas projection preserves semantic label and source metadata",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- matrix(rep(c(1, 1, 1), 6), ncol = 3, byrow = TRUE)
    projection <- make_toy_projection(coords)
    domain <- projection$specification$target
    sphere <- rbind(
      c(1, 0, 0),
      c(-1, 0, 0),
      c(0, 1, 0),
      c(0, -1, 0),
      c(0, 0, 1),
      c(0, 0, -1)
    )
    faces <- rbind(
      c(0, 2, 4),
      c(2, 1, 4),
      c(1, 3, 4),
      c(3, 0, 4),
      c(2, 0, 5),
      c(1, 2, 5),
      c(3, 1, 5),
      c(0, 3, 5)
    )
    geometry <- surface_geometry(domain, sphere, faces, rep(TRUE, 6))
    grid <- neuroim2::NeuroSpace(c(4, 4, 4), trans = diag(4))
    atlas <- new_atlas(
      "toy",
      neuroim2::NeuroVol(array(2, c(4, 4, 4)), grid),
      ids = 1:2,
      labels = c("A", "B"),
      orig_labels = c("A", "B"),
      hemi = c("left", "right"),
      cmap = matrix(1, 2, 3),
      ref = new_atlas_ref("toy", "two-regions", template_space = "toy-RAS")
    )
    local_mocked_bindings(
      get_template_transform = function(from, to, ...) {
        expect_identical(from, "toy-RAS")
        expect_identical(to$domain$id, domain$id)
        projection
      },
      .package = "neuroatlas"
    )
    result <- transform_atlas(atlas, "toy_6v", target = geometry)
    expect_equal(result$values, rep(2, 6))
    expect_equal(result$label_table$key, c(0, 1, 2))
    expect_identical(result$provenance$source_atlas_ref, atlas_ref(atlas))
    expect_identical(
      result$provenance$source_resource_metadata,
      atlas_metadata(
        atlas
      )
    )
    expect_error(
      transform_atlas(atlas, "fsaverage", target = geometry),
      "does not match"
    )
  }
)

test_that(
  "clustered volumes materialize ordered labels and background",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    coords <- rbind(
      c(0, 0, 0),
      c(1, 1, 1),
      c(2, 2, 2),
      c(3, 3, 3),
      c(1, 1, 1),
      c(2, 2, 2)
    )
    grid <- neuroim2::NeuroSpace(c(4, 4, 4), trans = diag(4))
    mask <- array(FALSE, c(4, 4, 4))
    mask[2, 2, 2] <- TRUE
    mask[3, 3, 3] <- TRUE
    volume <- neuroim2::ClusteredNeuroVol(
      neuroim2::LogicalNeuroVol(mask, grid),
      clusters = c(1L, 2L)
    )
    result <- apply_surface_projection(
      volume,
      make_toy_projection(coords),
      "toy-RAS",
      data_type = "label"
    )
    expect_equal(result$values, c(0, 1, 2, 0, 1, 2))
    expect_true(all(result$coverage$available))
  }
)

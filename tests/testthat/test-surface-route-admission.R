make_toy_admitted_surface <- function(template, hemisphere = "L") {
  vertices <- rbind(
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
  density <- if (template == "fsaverage") "164k" else "32k"
  mask <- rep(TRUE, 6)
  domain <- surface_domain(
    template,
    hemisphere,
    density,
    vertices,
    faces,
    mask,
    "analytic",
    "toy"
  )
  surface_geometry(domain, vertices, faces, mask)
}

make_toy_admission_row <- function(from, to) {
  registry <- space_transform_manifest()
  row <- registry[which(registry$backend == "neurotransform_native")[[1L]], ]
  row$from_domain_id <- from$domain$id
  row$to_domain_id <- to$domain$id
  row
}

test_that("exact surface identity is verified and replayed", {
  skip_if_not_installed("neurotransform", "0.2.0")
  geometry <- make_toy_admitted_surface("fsaverage")
  local_mocked_bindings(
    .surface_engine_revision_verified = function(revision) TRUE,
    .package = "neuroatlas"
  )
  cache <- tempfile("surface-identity-")
  transform <- get_template_transform(geometry, geometry, cache_dir = cache)
  expect_identical(transform$specification$qualification, "passed")
  expect_identical(transform$specification$route_id, "exact_index_identity")
  expect_identical(
    transform,
    get_template_transform(
      geometry, geometry, cache_dir = cache, offline = TRUE
    )
  )
  for (kind in c("continuous", "label", "probability")) {
    values <- if (kind == "probability") {
      cbind(rep(0.25, 6), rep(0.5, 6))
    } else if (kind == "label") {
      c(0, 11, 12, 0, 11, 12)
    } else {
      c(-1, 0, 1, 2, 3, 4)
    }
    x <- surface_data(values, geometry$domain, kind)
    result <- apply_template_transform(x, transform)
    expect_equal(result$values, x$values)
    expect_true(all(result$coverage$available))
  }
})

test_that("identity admission rejects a nonidentity operator", {
  skip_if_not_installed("neurotransform", "0.2.0")
  geometry <- make_toy_admitted_surface("fsaverage")
  operator <- get_surface_transform(geometry, geometry, cache_dir = tempfile())
  operator$plan$cols <- c(2:6, 1L)
  operator$integrity <- neuroatlas:::.surface_transform_digest(operator)
  expect_silent(neuroatlas:::.validate_surface_transform(operator))
  local_mocked_bindings(
    get_surface_transform = function(...) operator,
    .surface_engine_revision_verified = function(revision) TRUE,
    .package = "neuroatlas"
  )
  expect_error(
    get_template_transform(geometry, geometry, cache_dir = tempfile()),
    "does not establish exact index identity"
  )
})

test_that("identity admission requires the pinned engine build", {
  skip_if_not_installed("neurotransform", "0.2.0")
  geometry <- make_toy_admitted_surface("fsaverage")
  local_mocked_bindings(
    .surface_engine_revision_verified = function(revision) FALSE,
    .package = "neuroatlas"
  )
  expect_error(
    get_template_transform(geometry, geometry, cache_dir = tempfile()),
    "qualified pinned engine build"
  )
})

test_that(
  "names and other exact domains cannot inherit qualification",
  {
    from <- make_toy_admitted_surface("fsaverage")
    to <- make_toy_admitted_surface("fsLR")
    expect_error(
      atlas_transform_plan(
        "fsaverage",
        "fsLR_32k",
        available_only = TRUE,
        mode = "strict"
      ),
      "No transform route"
    )
    expect_error(
      atlas_transform_plan(
        from$domain,
        to$domain,
        available_only = TRUE,
        mode = "strict"
      ),
      "No transform route"
    )
    registry <- make_toy_admission_row(from, to)
    local_mocked_bindings(
      .space_transform_registry = function() registry,
      .package = "neuroatlas"
    )
    plan <- atlas_transform_plan(
      from$domain,
      to$domain,
      data_type = "vertex",
      available_only = TRUE,
      mode = "strict"
    )
    expect_identical(plan$status, "available")
    expect_identical(plan$from_domain$id, from$domain$id)
    changed <- surface_domain(
      "fsaverage",
      "L",
      "164k",
      from$sphere,
      from$triangles,
      c(FALSE, rep(TRUE, 5)),
      "analytic",
      "toy"
    )
    expect_error(
      atlas_transform_plan(
        changed,
        to$domain,
        available_only = TRUE,
        mode = "strict"
      ),
      "No transform route"
    )
    expect_error(
      atlas_transform_plan(
        from$domain,
        make_toy_admitted_surface("fsLR", "R")$domain,
        mode = "strict"
      ),
      "same hemisphere"
    )
  }
)

test_that(
  "public template workflow executes only an admitted exact surface route",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    from <- make_toy_admitted_surface("fsaverage")
    to <- make_toy_admitted_surface("fsLR")
    registry <- make_toy_admission_row(from, to)
    cache <- tempfile("admitted-surface-")
    on.exit(unlink(cache, recursive = TRUE), add = TRUE)
    local_mocked_bindings(
      .space_transform_registry = function() registry,
      .surface_engine_revision_verified = function(...) TRUE,
      .package = "neuroatlas"
    )
    operator <- get_template_transform(from, to, cache_dir = cache)
    expect_identical(operator$specification$qualification, "passed")
    expect_identical(
      get_template_transform(
        from,
        to,
        cache_dir = cache,
        offline = TRUE
      ),
      operator
    )
    x <- surface_data(c(0, 1, 2, 3, 4, 5), from$domain)
    y <- apply_template_transform(x, operator)
    expect_equal(y$values, x$values)
    expect_identical(y$domain$id, to$domain$id)
    expect_error(apply_template_transform(x, operator, from$domain), "Target domain")
    expect_error(
      apply_template_transform(x, operator, data_type = "label"),
      "matching domain-bound"
    )
    expect_error(
      apply_template_transform(x, operator, interpolation = "nearest"),
      "fixed by"
    )
  }
)

test_that(
  "an unbound engine cannot promote a qualified domain tuple",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    from <- make_toy_admitted_surface("fsaverage")
    to <- make_toy_admitted_surface("fsLR")
    registry <- make_toy_admission_row(from, to)
    local_mocked_bindings(
      .space_transform_registry = function() registry,
      .surface_engine_revision_verified = function(...) FALSE,
      .package = "neuroatlas"
    )
    expect_identical(
      get_surface_transform(from, to, NULL)$specification$qualification,
      "unqualified"
    )
    expect_error(
      get_template_transform(from, to, cache_dir = tempfile()),
      "qualified pinned engine"
    )
  }
)

test_that(
  "engine admission requires immutable revision or complete build binding",
  {
    skip_if_not_installed("neurotransform", "0.2.0")
    revision <- "933edddda462593941e167726e8aaa7168ff103a"
    local_mocked_bindings(
      .neurotransform_source_sha = function() NA_character_,
      .package = "neuroatlas"
    )
    withr::local_envvar(NEUROATLAS_ENGINE_BINDING = tempfile())
    expect_false(neuroatlas:::.surface_engine_revision_verified(revision))
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    writeLines('{"schema":"neuroatlas.engine-build.v1","version":"0.2.0"}', path)
    withr::local_envvar(NEUROATLAS_ENGINE_BINDING = path)
    expect_false(neuroatlas:::.surface_engine_revision_verified(revision))
  }
)

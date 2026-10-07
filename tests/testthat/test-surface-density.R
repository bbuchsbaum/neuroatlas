read_density_test_domain <- function(density, hemi) {
  catalog <- neuroatlas:::.surface_input_json(if (density == "164k") {
    "surface-domains-v1.json"
  } else {
    "surface-density-domains-v1.json"
  })
  domain <- catalog$domains[[paste("fsaverage", density, hemi, sep = "-")]]$domain
  structure(domain, class = c("SurfaceDomain", "list"))
}

test_that("density route admission binds direction, hemisphere and exact inputs", {
  skip_if_not_installed("jsonlite")
  for (hemi in c("L", "R")) {
    high <- read_density_test_domain("164k", hemi)
    for (density in c("41k", "10k")) {
      low <- read_density_test_domain(density, hemi)
      for (down in c(TRUE, FALSE)) {
        from <- if (down) high else low
        to <- if (down) low else high
        plan <- atlas_transform_plan(
          from, to, data_type = "vertex", available_only = TRUE, mode = "strict"
        )
        expect_identical(plan$status, "available")
        expect_equal(plan$n_steps, 1L)
        expect_identical(plan$steps$from_domain_id, from$id)
        expect_identical(plan$steps$to_domain_id, to$id)
        expect_identical(plan$steps$hemisphere, hemi)
        expect_identical(plan$steps$from_density, from$density)
        expect_identical(plan$steps$to_density, to$density)
        expect_identical(plan$steps$route_scope, "exact_surface_domains")
        expect_false(plan$steps$reversible)
        changed <- from
        changed$revision <- "different-inputs"
        changed$id <- neuroatlas:::.surface_descriptor_id(changed)
        expect_error(
          atlas_transform_plan(
            changed, to, available_only = TRUE, mode = "strict"
          ),
          "No transform route"
        )
      }
      other <- read_density_test_domain(density, if (hemi == "L") "R" else "L")
      expect_error(
        atlas_transform_plan(high, other, mode = "strict"), "same hemisphere"
      )
    }
  }
  for (low_name in c("fsaverage5", "fsaverage6")) {
    expect_error(
      get_template_transform("fsaverage", low_name), "No transform route"
    )
  }
})

test_that("manifest separates roadmap methods from executable identity scopes", {
  reg <- space_transform_manifest()
  roadmap <- reg$route_scope == "roadmap_placeholder"
  expect_true(any(roadmap))
  expect_true(all(reg$status[roadmap] == "planned"))
  expect_false(any(reg$executable[roadmap]))
  expect_true(all(is.na(reg$from_domain_id[roadmap])))
  expect_true(all(is.na(reg$to_domain_id[roadmap])))
  family <- reg$route_scope == "coordinate_family"
  expect_equal(sum(family), 2L)
  expect_true(all(reg$backend[family] == "internal_affine"))
  native <- reg$backend == "neurotransform_native"
  expect_true(all(reg$route_scope[native] == "exact_surface_domains"))
  expect_true(all(!is.na(reg$from_density[native])))
  expect_true(all(!is.na(reg$to_density[native])))
  density <- reg$artifact_version == "surface-density-transforms-v1"
  expect_equal(sum(density, na.rm = TRUE), 8L)
})

test_that("new input catalogs preserve released domains and lock all low meshes", {
  skip_if_not_installed("jsonlite")
  old <- neuroatlas:::.surface_input_json("surface-domains-v1.json")
  expect_identical(
    old$input_lock_sha256,
    "efcd4d03f1361ff3be350f5fa2f640b146677de5cd780ae2a5b523c6151c97e0"
  )
  low <- neuroatlas:::.surface_input_json("surface-density-domains-v1.json")
  expect_identical(low$input_lock_sha256, digest::digest(
    file = neuroatlas:::.surface_input_path("surface-density-inputs-v1.json"),
    algo = "sha256"
  ))
  expect_equal(length(low$domains), 4L)
  for (entry in low$domains) {
    domain <- structure(entry$domain, class = c("SurfaceDomain", "list"))
    expect_no_error(neuroatlas:::.validate_surface_domain(domain))
    expect_identical(domain$revision, low$input_lock_sha256)
    expect_true(domain$n_vertices %in% c(10242L, 40962L))
  }
})

test_that("density geometry verifies its own lock before reading any input", {
  skip_if_not_installed("gifti")
  skip_if_not_installed("jsonlite")
  sphere <- diag(3)
  triangles <- matrix(c(0L, 1L, 2L), nrow = 1)
  cortex <- c(TRUE, FALSE, TRUE)
  area <- c(1, 2, 3)
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeLines("density lock", path)
  lock_sha <- digest::digest(file = path, algo = "sha256")
  domain <- surface_domain(
    "fsaverage", "L", "10k", sphere, triangles,
    cortex, "fsaverage", lock_sha, vertex_area = area
  )
  assets <- list(sphere = "sphere.gii", mask = "mask.gii", area = "area.gii")
  entry <- list(domain = domain, assets = assets)
  catalog <- list(
    input_lock_sha256 = lock_sha, domains = list(`fsaverage-10k-L` = entry)
  )
  lock <- list(
    artifact_version = "surface-density-inputs-v1",
    assets = lapply(assets, function(x) list(path = x))
  )
  reads <- character()
  local_mocked_bindings(
    .surface_input_path = function(name) path,
    .surface_input_json = function(name) {
      switch(name,
        "surface-domains-v1.json" = list(domains = list()),
        "surface-density-domains-v1.json" = catalog,
        "surface-density-inputs-v1.json" = lock,
        stop("Unexpected manifest")
      )
    },
    .read_locked_surface_input = function(
      a, actual_lock, cache_dir, download, offline
    ) {
      expect_identical(actual_lock, lock)
      expect_false(download)
      expect_true(offline)
      reads <<- c(reads, a$path)
      switch(a$path,
        "sphere.gii" = list(data = list(pointset = sphere, triangle = triangles)),
        "mask.gii" = list(data = list(as.integer(cortex))),
        "area.gii" = list(data = list(area))
      )
    },
    .package = "neuroatlas"
  )
  geometry <- get_surface_geometry(
    "fsaverage", "10k", "L", cache_dir = tempfile(),
    download = FALSE, offline = TRUE
  )
  expect_identical(geometry$domain$id, domain$id)
  expect_identical(geometry$input_lock_sha256, lock_sha)
  expect_identical(geometry$cortex, cortex)
  expect_identical(reads, unname(unlist(assets)))
  reads <- character()
  writeLines("tampered density lock", path)
  expect_error(
    get_surface_geometry("fsaverage", "10k", "L"), "pinned identity"
  )
  expect_length(reads, 0L)
})

make_toy_surface_args <- function() {
  list(template = "toy", hemisphere = "L", density = "6v",
    sphere = rbind(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0), c(0, -1, 0),
                   c(0, 0, 1), c(0, 0, -1)),
    triangles = rbind(c(0, 2, 4), c(2, 1, 4), c(1, 3, 4), c(3, 0, 4),
                      c(2, 0, 5), c(1, 2, 5), c(3, 1, 5), c(0, 3, 5)),
    cortex = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    registration = "toy-frame-v1", revision = "toy-revision-1",
    vertex_area = rep(1, 6))
}

test_that("surface identity ignores R storage and indexing conventions", {
  args <- make_toy_surface_args()
  domain <- do.call(surface_domain, args)
  expect_s3_class(domain, "SurfaceDomain")
  expect_equal(domain$n_vertices, 6L)
  expect_match(domain$id, "^[0-9a-f]{64}$")
  args$triangles <- args$triangles + 1L
  args$index_base <- "one"
  storage.mode(args$sphere) <- "integer"
  rownames(args$sphere) <- letters[1:6]
  expect_identical(do.call(surface_domain, args), domain)
  plan <- atlas_transform_plan(domain, domain, data_type = "vertex")
  expect_identical(plan$confidence, "exact")
  expect_identical(plan$from_domain, domain)
  expect_error(atlas_transform_plan(domain, domain, data_type = "voxel"),
               "cannot define voxel")
  expect_error(atlas_transform_plan(domain, "toy"), "Invalid or modified")
})

test_that("domain fingerprints bind ordering, masks, revisions, and area", {
  original <- make_toy_surface_args()
  domain <- do.call(surface_domain, original)
  replacements <- list(
    sphere = original$sphere[6:1, ], triangles = original$triangles[8:1, ],
    cortex = !original$cortex, revision = "toy-revision-2",
    registration = "other-frame", vertex_area = rep(2, 6),
    density = "another-density", template = "another-template")
  for (field in names(replacements)) {
    args <- original
    args[[field]] <- replacements[[field]]
    changed <- do.call(surface_domain, args)
    expect_false(identical(changed$id, domain$id), info = field)
    if (field %in% c("sphere", "triangles", "cortex", "revision", "vertex_area")) {
      expect_error(atlas_transform_plan(domain, changed, mode = "strict"),
                   "Surface identity requires", info = field)
    }
  }
  args <- original
  args$hemisphere <- "R"
  expect_error(atlas_transform_plan(domain, do.call(surface_domain, args)),
               "same hemisphere")
  args$vertex_area <- NULL
  expect_true(is.na(do.call(surface_domain, args)$area_sha256))
  modified <- domain
  modified$revision <- "mutated"
  expect_error(atlas_transform_plan(domain, modified), "Invalid or modified")
})

test_that("surface constructors reject ambiguous or invalid arrays", {
  args <- make_toy_surface_args()
  cases <- list(
    hemisphere = "both", revision = NA_character_,
    cortex = rep(1, 6), vertex_area = rep(0, 6),
    triangles = matrix(c(0, 1, 6), nrow = 1))
  for (field in names(cases)) {
    bad <- args
    bad[[field]] <- cases[[field]]
    expect_error(do.call(surface_domain, bad), info = field)
  }
  args$triangles[1, 1] <- 0.5
  expect_error(do.call(surface_domain, args))
  args <- make_toy_surface_args()
  args$sphere[1, ] <- 0
  expect_error(do.call(surface_domain, args), "nonzero")
  args <- make_toy_surface_args()
  args$cortex[1] <- NA
  expect_error(do.call(surface_domain, args))
})

test_that("typed nonidentity plans retain domains without qualifying a route", {
  args <- make_toy_surface_args()
  source <- do.call(surface_domain, args)
  args$template <- "target"
  target <- do.call(surface_domain, args)
  registry <- space_transform_manifest()
  registry <- registry[registry$transform_type == "sphere_resample", ][1, ]
  registry$from_space <- "toy_6v"
  registry$to_space <- "target_6v"
  local_mocked_bindings(.space_transform_registry = function() registry,
                        .package = "neuroatlas")
  plan <- atlas_transform_plan(source, target, mode = "strict")
  expect_identical(plan$from_domain, source)
  expect_identical(plan$to_domain, target)
  expect_identical(plan$status, "planned")
  expect_false(any(plan$steps$executable))
  expect_true(any(grepl("not qualified", plan$warnings)))
  expect_error(atlas_transform_plan(source, target, mode = "strict",
                                    available_only = TRUE), "No transform route")
})

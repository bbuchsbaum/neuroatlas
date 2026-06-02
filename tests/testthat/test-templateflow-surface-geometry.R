test_that("surface template reader uses neurosurf result when available", {
  expected <- structure(list(path = "surface.surf.gii"), class = "surface_ok")

  out <- neuroatlas:::.read_surface_template_geometry(
    "surface.surf.gii",
    hemi = "L",
    geometry_reader = function(path) expected,
    gifti_reader = function(path) {
      stop("GIFTI fallback should not be called", call. = FALSE)
    }
  )

  expect_identical(out, expected)
})

test_that("surface template reader falls back to GIFTI arrays", {
  verts <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(c(0L, 1L, 2L, 0L, 2L, 3L), ncol = 3, byrow = TRUE)
  fake_gifti <- list(data = list(pointset = verts, triangle = faces))

  out <- neuroatlas:::.read_surface_template_geometry(
    "surface.surf.gii",
    hemi = "L",
    geometry_reader = function(path) {
      stop("$ operator is invalid for atomic vectors", call. = FALSE)
    },
    gifti_reader = function(path) fake_gifti
  )

  expect_s4_class(out, "SurfaceGeometry")
  expect_equal(out@hemi, "left")
})

test_that("surface template reader reports fallback failures clearly", {
  expect_error(
    neuroatlas:::.read_surface_template_geometry(
      "surface.surf.gii",
      hemi = "L",
      geometry_reader = function(path) {
        stop("$ operator is invalid for atomic vectors", call. = FALSE)
      },
      gifti_reader = function(path) list(data = list(pointset = matrix(1)))
    ),
    class = "neuroatlas_error_surface_template"
  )
})

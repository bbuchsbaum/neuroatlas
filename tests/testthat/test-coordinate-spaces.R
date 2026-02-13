# Tests for coordinate space transforms

test_that("MNI305_to_MNI152 matrix has correct dimensions", {
  expect_true(is.matrix(MNI305_to_MNI152))
  expect_equal(dim(MNI305_to_MNI152), c(4, 4))
})

test_that("MNI152_to_MNI305 matrix has correct dimensions", {
  expect_true(is.matrix(MNI152_to_MNI305))
  expect_equal(dim(MNI152_to_MNI305), c(4, 4))
})

test_that("MNI152_to_MNI305 is the inverse of MNI305_to_MNI152", {
  product <- MNI305_to_MNI152 %*% MNI152_to_MNI305
  expect_equal(product, diag(4), tolerance = 1e-10)
})

test_that("canonical test point transforms correctly", {
  # From FreeSurfer docs: (10, -20, 35) in MNI305 -> (10.695, -18.409, 36.137) in MNI152
  point_305 <- c(10, -20, 35)
  expected_152 <- c(10.695, -18.409, 36.137)

  result <- transform_coords(point_305, from = "MNI305", to = "MNI152")

  expect_equal(result, expected_152, tolerance = 0.001)
})

test_that("round-trip transform preserves coordinates", {
  original <- c(10, -20, 35)

  transformed <- transform_coords(original, from = "MNI305", to = "MNI152")
  round_trip <- transform_coords(transformed, from = "MNI152", to = "MNI305")

  expect_equal(round_trip, original, tolerance = 1e-10)
})

test_that("transform_coords handles matrix input", {
  points <- rbind(
    c(10, -20, 35),
    c(0, 0, 0),
    c(-30, 15, 50)
  )

  result <- transform_coords(points, from = "MNI305", to = "MNI152")

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))

  # First row should match canonical test
  expect_equal(result[1, ], c(10.695, -18.409, 36.137), tolerance = 0.001)
})

test_that("transform_coords handles column-major input", {
  points <- rbind(
    c(10, -20, 35),
    c(0, 0, 0)
  )
  points_cols <- t(points)  # 3 x 2 matrix

  result <- transform_coords(points_cols, from = "MNI305", to = "MNI152",
                             coords_as_cols = TRUE)

  expect_equal(dim(result), c(3, 2))
  expect_equal(result[, 1], c(10.695, -18.409, 36.137), tolerance = 0.001)
})

test_that("origin transforms with expected translation", {
  origin <- c(0, 0, 0)
  result <- transform_coords(origin, from = "MNI305", to = "MNI152")

  # The translation component of the matrix
  expect_equal(result, c(-0.0429, 1.5496, 1.1840), tolerance = 1e-4)
})

test_that("same-space transform returns identity", {
  point <- c(10, -20, 35)

  result_305 <- transform_coords(point, from = "MNI305", to = "MNI305")
  result_152 <- transform_coords(point, from = "MNI152", to = "MNI152")

  expect_equal(result_305, point)
  expect_equal(result_152, point)
})

test_that("get_space_transform returns correct matrices", {
  expect_equal(get_space_transform("MNI305", "MNI152"), MNI305_to_MNI152)
  expect_equal(get_space_transform("MNI152", "MNI305"), MNI152_to_MNI305)
  expect_equal(get_space_transform("MNI305", "MNI305"), diag(4))
  expect_equal(get_space_transform("MNI152", "MNI152"), diag(4))
})

test_that("get_space_transform handles case-insensitive input", {
  expect_equal(get_space_transform("mni305", "mni152"), MNI305_to_MNI152)
  expect_equal(get_space_transform("Mni305", "Mni152"), MNI305_to_MNI152)
})

test_that("get_space_transform warns on unknown space", {
  expect_warning(get_space_transform("unknown", "MNI152"), "Unknown source space")
  expect_warning(get_space_transform("MNI305", "unknown"), "Unknown target space")
})

test_that("get_surface_coordinate_space returns correct spaces", {
  expect_equal(get_surface_coordinate_space("fsaverage"), "MNI305")
  expect_equal(get_surface_coordinate_space("fsaverage5"), "MNI305")
  expect_equal(get_surface_coordinate_space("fsaverage6"), "MNI305")
  expect_equal(get_surface_coordinate_space("fsLR"), "MNI152")
  expect_equal(get_surface_coordinate_space("fslr"), "MNI152")  # case-insensitive
})

test_that("get_surface_coordinate_space warns on unknown template", {
  expect_warning(
    result <- get_surface_coordinate_space("unknown_template"),
    "Unknown surface template"
  )
  expect_equal(result, "Unknown")
})

test_that("needs_transform returns correct values", {
  expect_true(needs_transform("fsaverage", "MNI152"))
  expect_true(needs_transform("fsaverage6", "MNI152"))
  expect_false(needs_transform("fsaverage", "MNI305"))
  expect_false(needs_transform("fsLR", "MNI152"))
  expect_true(needs_transform("fsLR", "MNI305"))
})

test_that("transform_vertices_to_volume works correctly", {
  vertices <- rbind(
    c(10, -20, 35),
    c(0, 0, 0)
  )

  # fsaverage to MNI152: should transform
  result <- transform_vertices_to_volume(vertices, "fsaverage", "MNI152")
  expect_false(all(result == vertices))
  expect_equal(result[1, ], c(10.695, -18.409, 36.137), tolerance = 0.001)

  # fsLR to MNI152: should NOT transform (same space)
  result_fslr <- transform_vertices_to_volume(vertices, "fsLR", "MNI152")
  expect_equal(result_fslr, vertices)

  # fsaverage to MNI305: should NOT transform (same space)
  result_same <- transform_vertices_to_volume(vertices, "fsaverage", "MNI305")
  expect_equal(result_same, vertices)
})

test_that("coord_spaces list contains expected values", {
  expect_equal(coord_spaces$MNI305, "MNI305")
  expect_equal(coord_spaces$MNI152, "MNI152")
  expect_equal(coord_spaces$SCANNER, "Scanner")
  expect_equal(coord_spaces$UNKNOWN, "Unknown")
})

test_that("transform_coords validates input", {
  # Vector must have 3 elements (need to provide from/to to get past that check first)
  expect_error(transform_coords(c(1, 2), from = "MNI305", to = "MNI152"),
               "exactly 3 elements")
  # Matrix must have 3 columns
  expect_error(transform_coords(matrix(1:4, nrow = 2), from = "MNI305", to = "MNI152"),
               "3 columns")
  # Must provide from/to or transform
  expect_error(transform_coords(c(1, 2, 3)), "Must provide either")
})

test_that("direct transform matrix application works", {
  point <- c(10, -20, 35)

  # Using from/to
  result1 <- transform_coords(point, from = "MNI305", to = "MNI152")

  # Using explicit transform matrix
  result2 <- transform_coords(point, transform = MNI305_to_MNI152)

  expect_equal(result1, result2)
})

test_that("template_to_coord_space handles volumetric templates", {
  expect_equal(template_to_coord_space("MNI152NLin6Asym"), "MNI152")
  expect_equal(template_to_coord_space("MNI152NLin2009cAsym"), "MNI152")
  expect_equal(template_to_coord_space("MNI152"), "MNI152")
  expect_equal(template_to_coord_space("MNI305"), "MNI305")
})

test_that("template_to_coord_space handles surface templates", {
  expect_equal(template_to_coord_space("fsaverage"), "MNI305")
  expect_equal(template_to_coord_space("fsaverage5"), "MNI305")
  expect_equal(template_to_coord_space("fsaverage6"), "MNI305")
  expect_equal(template_to_coord_space("fsLR_32k"), "MNI152")
})

test_that("template_to_coord_space handles aliases", {
  expect_equal(template_to_coord_space("fslr32k"), "MNI152")
  expect_equal(template_to_coord_space("fslr"), "MNI152")
})

test_that("template_to_coord_space warns on unknown", {
  expect_warning(
    result <- template_to_coord_space("unknown_space"),
    "Unknown template"
  )
  expect_equal(result, "Unknown")
})

test_that("needs_coord_transform detects cross-coord-system cases", {
  expect_true(needs_coord_transform("fsaverage", "MNI152NLin6Asym"))
  expect_true(needs_coord_transform("fsaverage6", "MNI152"))
  expect_true(needs_coord_transform("MNI305", "MNI152NLin2009cAsym"))
})

test_that("needs_coord_transform returns FALSE for same coord system", {
  expect_false(needs_coord_transform("fsaverage", "MNI305"))
  expect_false(needs_coord_transform("fsaverage", "fsaverage6"))
  expect_false(needs_coord_transform("MNI152NLin6Asym", "MNI152NLin2009cAsym"))
  expect_false(needs_coord_transform("MNI152", "MNI152NLin6Asym"))
})

test_that("needs_template_warp detects same-coord-different-template", {
  expect_true(needs_template_warp("MNI152NLin6Asym", "MNI152NLin2009cAsym"))
  expect_true(needs_template_warp("fsaverage", "fsaverage6"))
  expect_true(needs_template_warp("fsaverage", "fsaverage5"))
})

test_that("needs_template_warp returns FALSE for identical templates", {
  expect_false(needs_template_warp("MNI152NLin6Asym", "MNI152NLin6Asym"))
  expect_false(needs_template_warp("fsaverage", "fsaverage"))
})

test_that("needs_template_warp returns FALSE for cross-coord-system", {
  expect_false(needs_template_warp("fsaverage", "MNI152"))
  expect_false(needs_template_warp("MNI305", "MNI152NLin6Asym"))
})

test_that("needs_template_warp returns NA for unknown templates", {
  expect_warning(
    result <- needs_template_warp("unknown", "MNI152"),
    "unknown template"
  )
  expect_true(is.na(result))
})

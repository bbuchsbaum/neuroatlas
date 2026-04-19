# Tests for plot_brain overlay colour mapping and layout-bookkeeping
# helpers extracted into R/plot_brain_overlays.R and R/plot_brain_layout.R.
# These are the small, pure pieces that are cheap to exercise without a
# real surface atlas.

# ---- .overlay_value_to_hex --------------------------------------------------

test_that(".overlay_value_to_hex returns NA for all-NA input", {
  skip_if_not_installed("scico")
  out <- .overlay_value_to_hex(c(NA_real_, NA_real_, NA_real_))
  expect_true(all(is.na(out)))
})

test_that(".overlay_value_to_hex returns hex strings for finite input", {
  skip_if_not_installed("scico")
  out <- .overlay_value_to_hex(c(-2, 0, 2))
  expect_length(out, 3L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6,8}$", out)))
})

test_that(".overlay_value_to_hex preserves NA at the same indices", {
  skip_if_not_installed("scico")
  vals <- c(-1, NA_real_, 0, 1, NA_real_)
  out <- .overlay_value_to_hex(vals)
  expect_true(is.na(out[2]))
  expect_true(is.na(out[5]))
  expect_false(any(is.na(out[c(1, 3, 4)])))
})

test_that(".overlay_value_to_hex clamps out-of-range values to the palette ends", {
  skip_if_not_installed("scico")
  lim <- c(-1, 1)
  # Values at the limits and far beyond should map to the palette endpoints
  at_min <- .overlay_value_to_hex(-1, lim = lim)
  beyond_min <- .overlay_value_to_hex(-100, lim = lim)
  at_max <- .overlay_value_to_hex(1, lim = lim)
  beyond_max <- .overlay_value_to_hex(100, lim = lim)
  expect_equal(at_min, beyond_min)
  expect_equal(at_max, beyond_max)
})

test_that(".overlay_value_to_hex copes with a degenerate constant range", {
  skip_if_not_installed("scico")
  out <- .overlay_value_to_hex(c(3, 3, 3))
  expect_length(out, 3L)
  expect_true(all(!is.na(out)))
})


# ---- .normalize_colorbar_position ------------------------------------------

test_that(".normalize_colorbar_position maps TRUE/FALSE and strings", {
  expect_equal(.normalize_colorbar_position(TRUE), "right")
  expect_equal(.normalize_colorbar_position(TRUE, default = "bottom"), "bottom")
  expect_equal(.normalize_colorbar_position(FALSE), "none")
  expect_equal(.normalize_colorbar_position("Bottom"), "bottom")
  expect_equal(.normalize_colorbar_position("none"), "none")
})

test_that(".normalize_colorbar_position rejects invalid input", {
  expect_error(.normalize_colorbar_position(NA))
  expect_error(.normalize_colorbar_position("top"))
  expect_error(.normalize_colorbar_position(c("right", "bottom")))
})


# ---- .resolve_plot_brain_panel_labels ---------------------------------------

test_that(".resolve_plot_brain_panel_labels returns defaults when NULL", {
  levels <- c("Left Lateral", "Right Medial")
  expect_equal(.resolve_plot_brain_panel_labels(levels, NULL), levels)
})

test_that(".resolve_plot_brain_panel_labels applies a function to each level", {
  levels <- c("Left Lateral", "Right Medial")
  out <- .resolve_plot_brain_panel_labels(levels, toupper)
  expect_equal(out, c("LEFT LATERAL", "RIGHT MEDIAL"))
})

test_that(".resolve_plot_brain_panel_labels uses unnamed vectors positionally", {
  levels <- c("Left Lateral", "Right Medial")
  out <- .resolve_plot_brain_panel_labels(levels, c("LL", "RM"))
  expect_equal(out, c("LL", "RM"))
})

test_that(".resolve_plot_brain_panel_labels uses named vectors selectively", {
  levels <- c("Left Lateral", "Right Medial", "Left Medial")
  out <- .resolve_plot_brain_panel_labels(
    levels,
    c(`Left Lateral` = "LL", `Left Medial` = "LM")
  )
  # Overrides for matched keys, defaults for the rest
  expect_equal(out[1], "LL")
  expect_equal(out[2], "Right Medial")
  expect_equal(out[3], "LM")
})

test_that(".resolve_plot_brain_panel_labels rejects length-mismatched vectors", {
  expect_error(
    .resolve_plot_brain_panel_labels(c("a", "b", "c"), c("x", "y")),
    "must have length 3"
  )
})


# ---- .compute_panel_layout_transforms --------------------------------------

test_that(".compute_panel_layout_transforms returns NULL for native layout", {
  poly <- tibble::tibble(
    panel = "A", view = "lateral",
    x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)
  )
  expect_null(.compute_panel_layout_transforms(poly, "native"))
})

test_that(".compute_panel_layout_transforms produces one row per panel", {
  poly <- tibble::tibble(
    panel = rep(c("A", "B"), each = 4),
    view = rep(c("lateral", "lateral"), each = 4),
    x = c(0, 1, 1, 0, 10, 12, 12, 10),
    y = c(0, 0, 1, 1, 5, 5, 8, 8)
  )
  tr <- .compute_panel_layout_transforms(poly, "presentation")
  expect_s3_class(tr, "tbl_df")
  expect_setequal(tr$panel, c("A", "B"))
  # Centres: A centred at (0.5, 0.5), B centred at (11, 6.5).
  expect_equal(tr$cx[tr$panel == "A"], 0.5)
  expect_equal(tr$cy[tr$panel == "A"], 0.5)
  expect_equal(tr$cx[tr$panel == "B"], 11)
  expect_equal(tr$cy[tr$panel == "B"], 6.5)
  # Larger-extent panel B gets a smaller scale factor than A.
  expect_lt(tr$scale[tr$panel == "B"], tr$scale[tr$panel == "A"])
})

test_that(".compute_panel_layout_transforms flags dorsal/ventral as rotated", {
  poly <- tibble::tibble(
    panel = c("D", "V", "L"),
    view = c("dorsal", "ventral", "lateral"),
    x = c(0, 0, 0), y = c(0, 0, 0)
  )
  tr <- .compute_panel_layout_transforms(poly, "presentation")
  expect_equal(tr$rotate[tr$panel == "D"], TRUE)
  expect_equal(tr$rotate[tr$panel == "V"], TRUE)
  expect_equal(tr$rotate[tr$panel == "L"], FALSE)
})


# ---- .apply_panel_layout_to_points ------------------------------------------

test_that(".apply_panel_layout_to_points translates and scales per panel", {
  poly <- tibble::tibble(
    panel = c("A", "A", "A", "A"),
    view = "lateral",
    x = c(0, 2, 2, 0),
    y = c(0, 0, 2, 2)
  )
  tr <- .compute_panel_layout_transforms(poly, "presentation")
  out <- .apply_panel_layout_to_points(poly, tr)
  # Centred at origin after transform.
  expect_equal(mean(out$x), 0, tolerance = 1e-10)
  expect_equal(mean(out$y), 0, tolerance = 1e-10)
  # Max extent scaled to about 0.5 (half of a unit-span bounding box).
  expect_lte(max(abs(out$x)), 0.5 + 1e-10)
  expect_lte(max(abs(out$y)), 0.5 + 1e-10)
})

test_that(".apply_panel_layout_to_points is a no-op when transforms is NULL", {
  poly <- tibble::tibble(panel = "A", x = 1, y = 2)
  expect_identical(
    .apply_panel_layout_to_points(poly, NULL),
    poly
  )
})

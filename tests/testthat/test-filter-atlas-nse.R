# Tests for filter_atlas() tidy-eval / NSE edge cases.
#
# These use a small synthetic atlas built via new_atlas() so the tests
# run without network or package data, and cover the rlang machinery
# (enquos/eval_tidy, .dots forwarding) as well as downstream contracts
# (empty result, class preservation, NA handling, unknown columns).

# ---- Helpers ---------------------------------------------------------------

.make_nse_toy_atlas <- function() {
  sp <- neuroim2::NeuroSpace(dim = c(5, 5, 5), spacing = c(1, 1, 1))
  arr <- array(0L, dim = c(5, 5, 5))
  arr[1:2, 1:2, 1:2] <- 1L
  arr[3:4, 1:2, 1:2] <- 2L
  arr[1:2, 3:4, 1:2] <- 3L
  arr[3:4, 3:4, 1:2] <- 4L

  mask_vol <- neuroim2::LogicalNeuroVol(arr != 0L, sp)
  clusters <- arr[arr != 0L]
  cvol <- neuroim2::ClusteredNeuroVol(
    mask = mask_vol,
    clusters = clusters,
    label_map = list(A1 = 1L, A2 = 2L, B1 = 3L, B2 = 4L)
  )

  ref <- new_atlas_ref(
    family = "toy", model = "NSEToy",
    representation = "volume",
    template_space = "custom", coord_space = "MNI152",
    confidence = "high"
  )

  new_atlas(
    name = "NSEToy",
    atlas = cvol,
    ids = 1:4,
    labels = c("A1", "A2", "B1", "B2"),
    orig_labels = c("lh_A1", "rh_A2", "lh_B1", "rh_B2"),
    hemi = c("left", "right", "left", "right"),
    network = c("DMN", "DMN", "Vis", "Vis"),
    cmap = data.frame(red = c(255, 0, 0, 0), green = c(0, 255, 0, 0),
                      blue = c(0, 0, 255, 0)),
    subclass = "toy",
    ref = ref
  )
}


# ---- Basic filtering --------------------------------------------------------

test_that("filter_atlas with no conditions returns an atlas with all regions", {
  atlas <- .make_nse_toy_atlas()
  out <- filter_atlas(atlas)
  expect_s3_class(out, "atlas")
  expect_equal(length(out$ids), length(atlas$ids))
})

test_that("filter_atlas by hemi returns only matching regions", {
  atlas <- .make_nse_toy_atlas()
  out <- filter_atlas(atlas, hemi == "left")
  expect_equal(out$ids, c(1L, 3L))
  expect_true(all(out$hemi == "left"))
  # Class preserved end-to-end.
  expect_s3_class(out, "toy")
  expect_s3_class(out, "atlas")
})

test_that("filter_atlas combines conditions via commas (AND)", {
  atlas <- .make_nse_toy_atlas()
  out <- filter_atlas(atlas, hemi == "left", network == "DMN")
  expect_equal(out$ids, 1L)
  expect_equal(out$labels, "A1")
})

test_that("filter_atlas supports & and | inside a single condition", {
  atlas <- .make_nse_toy_atlas()
  out_and <- filter_atlas(atlas, hemi == "right" & network == "Vis")
  expect_equal(out_and$ids, 4L)

  out_or <- filter_atlas(atlas, network == "DMN" | label == "B2")
  expect_setequal(out_or$ids, c(1L, 2L, 4L))
})


# ---- Richer NSE expressions -------------------------------------------------

test_that("filter_atlas works with %in% against a vector", {
  atlas <- .make_nse_toy_atlas()
  want <- c("A1", "B2")
  out <- filter_atlas(atlas, label %in% want)
  expect_setequal(out$labels, want)
})

test_that("filter_atlas works with function calls like grepl() and startsWith()", {
  atlas <- .make_nse_toy_atlas()
  out_g <- filter_atlas(atlas, grepl("^A", label))
  expect_setequal(out_g$ids, c(1L, 2L))

  out_s <- filter_atlas(atlas, startsWith(orig_labels, "lh_"))
  expect_true(all(out_s$hemi == "left"))
})

test_that("filter_atlas resolves external scalars via tidy evaluation", {
  atlas <- .make_nse_toy_atlas()
  target <- "Vis"
  out <- filter_atlas(atlas, network == target)
  expect_setequal(out$ids, c(3L, 4L))
})

test_that("filter_atlas threads the active atlas through a wrapping function", {
  # NSE correctness: a wrapper that forwards `...` should work without
  # manual quosure plumbing because filter_atlas uses rlang::enquos().
  wrapper <- function(x, ...) filter_atlas(x, ...)
  atlas <- .make_nse_toy_atlas()
  out <- wrapper(atlas, hemi == "left")
  expect_equal(out$ids, c(1L, 3L))
})


# ---- Standard-evaluation path via .dots -------------------------------------

test_that("filter_atlas accepts pre-built quosures via .dots", {
  atlas <- .make_nse_toy_atlas()
  qs <- rlang::quos(hemi == "right", network == "DMN")
  out <- filter_atlas(atlas, .dots = qs)
  expect_equal(out$ids, 2L)
})

test_that("filter_atlas prefers .dots when both ... and .dots are supplied", {
  atlas <- .make_nse_toy_atlas()
  qs <- rlang::quos(hemi == "left")
  # Condition in `...` would narrow to ids 3-4; .dots wins and narrows to 1,3.
  out <- filter_atlas(atlas, label %in% c("A2", "B1", "B2"), .dots = qs)
  expect_setequal(out$ids, c(1L, 3L))
})


# ---- NA handling -----------------------------------------------------------

test_that("filter_atlas treats NA predicates as FALSE", {
  atlas <- .make_nse_toy_atlas()
  # Inject NA hemi for id 1 in both the top-level field and the cached
  # roi_metadata tibble that filter_atlas inspects.
  atlas$hemi[1] <- NA_character_
  atlas$roi_metadata$hemi[1] <- NA_character_
  # `hemi == "left"` becomes NA for id 1, TRUE for id 3. NA -> FALSE.
  out <- filter_atlas(atlas, hemi == "left")
  expect_equal(out$ids, 3L)
})


# ---- Error paths -----------------------------------------------------------

test_that("filter_atlas errors when no rows match", {
  atlas <- .make_nse_toy_atlas()
  expect_error(
    filter_atlas(atlas, hemi == "nonexistent"),
    "no matching ROIs"
  )
})

test_that("filter_atlas errors for non-logical predicates", {
  atlas <- .make_nse_toy_atlas()
  expect_error(
    filter_atlas(atlas, label),
    "logical vectors"
  )
  expect_error(
    filter_atlas(atlas, nchar(label)),
    "logical vectors"
  )
})

test_that("filter_atlas errors on unknown columns", {
  atlas <- .make_nse_toy_atlas()
  expect_error(filter_atlas(atlas, nonexistent_col == "x"))
})


# ---- Downstream state -------------------------------------------------------

test_that("filter_atlas rebuilds roi_metadata for the kept regions", {
  atlas <- .make_nse_toy_atlas()
  out <- filter_atlas(atlas, network == "Vis")
  meta <- roi_metadata(out)
  expect_equal(nrow(meta), 2L)
  expect_setequal(meta$id, c(3L, 4L))
  expect_true(all(meta$network == "Vis"))
})

test_that("filter_atlas appends a history entry describing the subset", {
  atlas <- .make_nse_toy_atlas()
  n_before <- nrow(atlas_history(atlas))
  out <- filter_atlas(atlas, hemi == "left")
  hist <- atlas_history(out)
  expect_gt(nrow(hist), n_before)
  expect_true(any(hist$action == "subset"))
  # Details should reference the counts.
  subset_rows <- hist[hist$action == "subset", , drop = FALSE]
  expect_match(subset_rows$details[1], "Kept 2 of 4")
})

test_that("filter_atlas preserves underlying ClusteredNeuroVol structure", {
  atlas <- .make_nse_toy_atlas()
  out <- filter_atlas(atlas, hemi == "left")
  expect_s4_class(out$atlas, "ClusteredNeuroVol")
  # Only kept-ids appear in the clusters vector.
  expect_setequal(unique(out$atlas@clusters), c(1L, 3L))
})

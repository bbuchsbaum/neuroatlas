brainnetome_or_skip <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      testthat::skip(paste("Brainnetome download unavailable:", conditionMessage(e)))
    }
  )
}

test_that("Brainnetome label table includes expected parcel metadata", {
  skip_on_cran()

  labels <- brainnetome_or_skip(brainnetome_labels())

  expect_s3_class(labels, "tbl_df")
  expect_equal(nrow(labels), 246L)
  expect_equal(labels$id, seq_len(246L))
  expect_true(all(c(
    "label", "hemi", "lobe", "gyrus", "cytoarchitectonic_ref",
    "yeo_7network_name", "yeo_17network_name"
  ) %in% names(labels)))
  expect_equal(sum(labels$hemi == "left"), 123L)
  expect_equal(sum(labels$hemi == "right"), 123L)
  expect_false(any(is.na(labels$cytoarchitectonic_ref)))
  expect_equal(labels$label[[1]], "A8m_L")
  expect_match(labels$cytoarchitectonic_ref[[1]], "medial area 8")
})

test_that("Brainnetome atlas loads as a standard atlas with provenance", {
  skip_on_cran()

  atlas <- brainnetome_or_skip(get_brainnetome_atlas())

  expect_s3_class(atlas, "brainnetome")
  expect_s3_class(atlas, "atlas")
  expect_equal(length(atlas$ids), 246L)
  expect_equal(atlas$ids, seq_len(246L))

  meta <- roi_metadata(atlas)
  expect_true(all(c(
    "id", "label", "hemi", "network", "cytoarchitectonic_ref",
    "yeo_7network_name", "yeo_17network_name"
  ) %in% names(meta)))
  expect_equal(meta$label[[1]], "A8m_L")
  expect_match(meta$network[[1]], "Default")

  expect_equal(nrow(atlas_artifacts(atlas)), 4L)
  expect_equal(atlas_ref(atlas)$family, "brainnetome")
})

test_that("Brainnetome is registered for get_atlas dispatch", {
  specs <- list_atlases()

  expect_true("brainnetome" %in% specs$id)
  expect_s3_class(find_atlas_spec("bna"), "atlas_spec")
})

test_that("Brainnetome atlas-specific metadata survives filtering", {
  skip_on_cran()

  atlas <- brainnetome_or_skip(get_brainnetome_atlas())
  filtered <- filter_atlas(atlas, yeo_7network_name == "Default")
  meta <- roi_metadata(filtered)

  expect_s3_class(filtered, "brainnetome")
  expect_true("cytoarchitectonic_ref" %in% names(meta))
  expect_true(all(meta$yeo_7network_name == "Default"))
  expect_false(any(is.na(meta$cytoarchitectonic_ref)))
})

test_that("Brainnetome query_coord returns atlas-specific metadata", {
  skip_on_cran()

  atlas <- brainnetome_or_skip(get_brainnetome_atlas())
  mask <- methods::as(atlas$atlas@mask, "array")
  lin <- which(mask)[which(atlas$atlas@clusters == 1L)[[1]]]
  ijk <- arrayInd(lin, dim(mask))
  xyz <- neuroim2::grid_to_coord(neuroim2::space(atlas$atlas), ijk)
  q <- query_coord(atlas, xyz)

  expect_equal(q$id[[1]], 1L)
  expect_equal(q$label[[1]], "A8m_L")
  expect_true("cytoarchitectonic_ref" %in% names(q))
  expect_match(q$cytoarchitectonic_ref[[1]], "medial area 8")
})

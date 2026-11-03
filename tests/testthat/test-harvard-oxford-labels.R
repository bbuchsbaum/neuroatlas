# TemplateFlow Harvard-Oxford label tables (#59).

write_ho_tsv <- function(index, name) {
  path <- tempfile(fileext = ".tsv")
  utils::write.table(
    data.frame(index = index, name = name),
    path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  path
}

# Mean x (mm) of the voxels of each label; fails if a "Left" label lies at
# x > 0 or a "Right" label at x < 0.
expect_labels_on_their_hemisphere <- function(atlas) {
  vol <- atlas$atlas
  vals <- as.integer(as.vector(vol))
  sp <- neuroim2::space(vol)
  for (i in seq_along(atlas$ids)) {
    lab <- atlas$labels[i]
    if (!grepl("^(Left|Right) ", lab)) next
    idx <- which(vals == atlas$ids[i])
    mean_x <- mean(neuroim2::index_to_coord(sp, idx)[, 1])
    if (startsWith(lab, "Left")) {
      expect_lt(mean_x, 0, label = paste0("mean x of '", lab, "'"))
    } else {
      expect_gt(mean_x, 0, label = paste0("mean x of '", lab, "'"))
    }
  }
}

test_that("HOCPAL values map to 0-based table rows, missing rows filled", {
  # Rows 2-3 (Insular Cortex) are absent, as in TemplateFlow.
  tsv <- write_ho_tsv(
    c(0, 1, 4, 5),
    c(
      "Left Frontal Pole", "Right Frontal Pole",
      "Left Superior Frontal Gyrus", "Right Superior Frontal Gyrus"
    )
  )
  on.exit(unlink(tsv))
  info <- .harvard_oxford_tf_labels(tsv, 1:6, "HOCPAL")
  expect_equal(
    info$labels,
    c(
      "Left Frontal Pole", "Right Frontal Pole",
      "Left Insular Cortex", "Right Insular Cortex",
      "Left Superior Frontal Gyrus", "Right Superior Frontal Gyrus"
    )
  )
})

test_that("HOCPA values map to 0-based table rows, missing rows filled", {
  tsv <- write_ho_tsv(c(0, 1), c("Frontal Pole", "Insular Cortex"))
  on.exit(unlink(tsv))
  info <- .harvard_oxford_tf_labels(tsv, c(1L, 2L, 45L, 48L), "HOCPA")
  expect_equal(
    info$labels,
    c(
      "Frontal Pole", "Insular Cortex",
      "Heschl's Gyrus (includes H1 and H2)", "Occipital Pole"
    )
  )
})

test_that("HOSPA table (row 0 = Background) is matched directly", {
  tsv <- write_ho_tsv(
    0:2,
    c("Background", "Left Cerebral White Matter", "Left Cerebral Cortex")
  )
  on.exit(unlink(tsv))
  info <- .harvard_oxford_tf_labels(tsv, 1:3, "HOSPA")
  expect_equal(
    info$labels,
    c("Left Cerebral White Matter", "Left Cerebral Cortex", "Region3")
  )
})

test_that("a 1-based cortical table is not shifted", {
  tsv <- write_ho_tsv(1:2, c("Left Frontal Pole", "Right Frontal Pole"))
  on.exit(unlink(tsv))
  info <- .harvard_oxford_tf_labels(tsv, 1:2, "HOCPAL")
  expect_equal(info$labels, c("Left Frontal Pole", "Right Frontal Pole"))
})

test_that("missing table falls back to FSL names", {
  info <- .harvard_oxford_tf_labels(NULL, c(1L, 2L, 96L, 97L), "HOCPAL")
  expect_equal(
    info$labels,
    c("Left Frontal Pole", "Right Frontal Pole", "Right Occipital Pole",
      "Region97")
  )
})

test_that("HOCPAL loader puts Left labels at x < 0 and Right at x > 0", {
  # x runs from -4.5 to 4.5 mm; odd ids on the left, even on the right,
  # following TemplateFlow's dseg.
  arr <- array(0L, c(10, 3, 2))
  for (k in 1:3) {
    arr[1:5, k, ] <- 2L * k - 1L
    arr[6:10, k, ] <- 2L * k
  }
  sp <- neuroim2::NeuroSpace(
    dim = dim(arr), spacing = c(1, 1, 1), origin = c(-4.5, 0, 0)
  )
  vol <- neuroim2::NeuroVol(arr, sp)
  tsv <- write_ho_tsv(
    c(0, 1, 4, 5),
    c(
      "Left Frontal Pole", "Right Frontal Pole",
      "Left Superior Frontal Gyrus", "Right Superior Frontal Gyrus"
    )
  )
  on.exit(unlink(tsv))
  local_mocked_bindings(
    get_template = function(..., path_only = FALSE) {
      if (isTRUE(path_only)) tsv else vol
    }
  )

  a <- get_harvard_oxford_cortical_subcortical_atlas()
  expect_equal(a$ids, 1:6)
  expect_false(any(grepl("^Region", a$labels)))
  expect_equal(a$hemi, rep(c("left", "right"), 3))
  expect_labels_on_their_hemisphere(a)
})

test_that("TemplateFlow HOCPAL labels match voxel hemispheres", {
  skip_on_cran()
  skip_if_offline()
  a <- tryCatch(
    get_harvard_oxford_cortical_subcortical_atlas(resolution = "02"),
    error = function(e) {
      skip(paste("TemplateFlow unavailable:", conditionMessage(e)))
    }
  )
  expect_length(a$ids, 96)
  expect_false(any(grepl("^Region", a$labels)))
  expect_labels_on_their_hemisphere(a)
})

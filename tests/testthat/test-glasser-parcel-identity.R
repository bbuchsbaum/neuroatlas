make_toy_glasser_pair <- function() {
  areas <- c("V1", paste0("area", 2:179), "p24")
  label_path <- tempfile(fileext = ".txt")
  vol_path <- tempfile(fileext = ".nii")
  on.exit(unlink(c(label_path, vol_path)))
  writeLines(c(paste0("Right_", areas), paste0("Left_", areas)), label_path)
  vol <- neuroim2::NeuroVol(array(1:360, c(6, 6, 10)),
                           neuroim2::NeuroSpace(c(6, 6, 10)))
  neuroim2::write_vol(vol, vol_path)

  testthat::local_mocked_bindings(
    .neuroatlas_try_download = function(...) list(ok = TRUE, path = vol_path),
    .neuroatlas_download = function(...) label_path,
    .glasser_fsaverage_surface_hemi = function(hemi, ...) {
      geometry <- neurosurf::SurfaceGeometry(
        vert = rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0)),
        faces = matrix(c(0L, 1L, 2L), nrow = 1), hemi = hemi
      )
      methods::new(
        "LabeledNeuroSurface", geometry = geometry, indices = 1:3,
        labels = c("???", paste0(if (hemi == "lh") "L_" else "R_",
                                    areas, "_ROI")),
        cols = rep("#4477AA", 181), data = c(1, 2, 181)
      )
    },
    .package = "neuroatlas"
  )
  list(volume = get_glasser_atlas(), surface = glasser_surf())
}

test_that("Glasser loaders preserve IDs and share hemisphere-qualified keys", {
  pair <- make_toy_glasser_pair()
  v <- pair$volume
  s <- pair$surface
  vm <- roi_metadata(v)
  sm <- roi_metadata(s)
  edge <- c(1, 180, 181, 360)

  expect_identical(v$ids, 1:360)
  expect_identical(s$ids, 1:360)
  expect_equal(vm$hemi[edge], c("right", "right", "left", "left"))
  expect_equal(sm$hemi[edge], c("left", "left", "right", "right"))
  expect_equal(vm$label_full[edge],
               c("R_V1_ROI", "R_p24_ROI", "L_V1_ROI", "L_p24_ROI"))
  expect_equal(sm$label_full[edge],
               c("L_V1_ROI", "L_p24_ROI", "R_V1_ROI", "R_p24_ROI"))
  expect_setequal(vm$label_full, sm$label_full)
  expect_equal(vm$area, rep(c("V1", paste0("area", 2:179), "p24"), 2))
  expect_equal(sm$area, vm$area)
  expect_equal(v$orig_labels[1], "Right_V1")
  expect_equal(s$lh_atlas@data, c(0, 1, 180))
  expect_equal(s$rh_atlas@data, c(0, 181, 360))
  expect_equal(atlas_ref(v)$id_convention, "hcp_R_first")
  expect_equal(atlas_ref(s)$id_convention, "surfatlas_L_first")
  expect_true(all(vm$id_convention == "hcp_R_first"))
  expect_true(all(sm$id_convention == "surfatlas_L_first"))
  expect_equal(atlas_metadata(s)$identity$id_convention, "surfatlas_L_first")

  sub <- suppressWarnings(sub_atlas(v, ids = c(1, 181)))
  expect_equal(roi_metadata(sub)$label_full, c("R_V1_ROI", "L_V1_ROI"))
  expect_equal(atlas_ref(sub)$id_convention, "hcp_R_first")
})

test_that("Glasser ID joins reject missing and conflicting conventions", {
  pair <- make_toy_glasser_pair()
  for (atlas in pair) {
    values <- data.frame(id = 1:360, score = seq_len(360))
    expect_error(as_parcel_data(atlas, values = values, by = "id"),
                 class = "neuroatlas_error_id_convention")
    expect_error(align_parcel_values(atlas, values, score),
                 class = "neuroatlas_error_id_convention")
    names(values)[1] <- "roi_index"
    expect_error(align_parcel_values(atlas, values, score,
                                     by = c(id = "roi_index")),
                 class = "neuroatlas_error_id_convention")
    names(values)[1] <- "id"
    values$id_convention <- atlas_ref(atlas)$id_convention
    expect_equal(unname(align_parcel_values(atlas, values, score)), 1:360)
    expect_equal(as_parcel_data(atlas, values = values)$parcels$score, 1:360)
    values$id_convention[1] <- NA_character_
    expect_error(align_parcel_values(atlas, values, score),
                 class = "neuroatlas_error_id_convention")
    values$id_convention[1] <- "another_convention"
    expect_error(align_parcel_values(atlas, values, score),
                 class = "neuroatlas_error_id_convention")
  }
  values <- data.frame(id = 1:360, score = 1:360,
                       id_convention = "hcp_R_first")
  expect_error(align_parcel_values(pair$surface, values, score),
               class = "neuroatlas_error_id_convention")
  values$hemi <- pair$surface$hemi
  values$id_convention <- NULL
  expect_error(align_parcel_values(pair$surface, values, score,
                                   by = c("id", "hemi")),
               class = "neuroatlas_error_id_convention")
  expect_error(plot_brain(pair$surface, data = values, value = score,
                          interactive = FALSE),
               class = "neuroatlas_error_id_convention")
  expect_error(parcel_volume(pair$volume, values, score),
               class = "neuroatlas_error_id_convention")
})

test_that("Glasser shared keys transfer nonconstant values without a swap", {
  pair <- make_toy_glasser_pair()
  for (direction in list(c("volume", "surface"), c("surface", "volume"))) {
    source <- as_parcel_data(pair[[direction[1]]], values = 1:360)
    target <- pair[[direction[2]]]
    # Independent expected permutation: the first hemisphere moves last.
    expected <- c(181:360, 1:180)
    full <- source$parcels[360:1, c("label_full", "value")]
    expect_equal(unname(align_parcel_values(target, full, value)), expected)
    expect_equal(as_parcel_data(target, values = full,
                                by = "label_full")$parcels$value, expected)
    composite <- source$parcels[c("area", "hemi", "value")]
    expect_equal(unname(align_parcel_values(target, composite, value,
                                             by = c("area", "hemi"))),
                 expected)
    expect_error(as_parcel_data(target, values = source),
                 class = "neuroatlas_error_atlas_identity")
    # Supplying source IDs alongside a shared key cannot bypass verification.
    full$id <- source$parcels$id[360:1]
    expect_error(align_parcel_values(target, full, value, by = "label_full"),
                 class = "neuroatlas_error_parcel_metadata")
  }
})

test_that("Glasser convention survives parcel serialization and is checked", {
  atlas <- make_toy_glasser_pair()$volume
  source <- as_parcel_data(atlas, values = 1:360)
  expect_equal(source$atlas$id_convention, "hcp_R_first")
  for (format in c("rds", "json")) {
    path <- tempfile(fileext = paste0(".", format))
    write_parcel_data(source, path)
    restored <- read_parcel_data(path)
    unlink(path)
    expect_equal(restored$atlas$id_convention, "hcp_R_first")
    expect_equal(unname(parcel_values(restored, atlas)), 1:360)
  }
  source$parcels$id_convention <- NULL
  expect_equal(unname(align_parcel_values(atlas, source, value)), 1:360)
  source$atlas$id_convention <- "surfatlas_L_first"
  expect_error(align_parcel_values(atlas, source, value),
               class = "neuroatlas_error_id_convention")
  source$atlas$id_convention <- NULL
  expect_error(align_parcel_values(atlas, source, value),
               class = "neuroatlas_error_id_convention")

  legacy <- atlas
  legacy$metadata$identity$id_convention <- NULL
  legacy$atlas_ref$id_convention <- NULL
  expect_error(align_parcel_values(legacy,
                                   as_parcel_data(atlas, values = 1:360), value),
               class = "neuroatlas_error_id_convention")
})

make_toy_metadata_atlas <- function(family = "schaefer", shift = 0) {
  sp <- neuroim2::NeuroSpace(c(5, 5, 5), spacing = c(1, 1, 1),
                            origin = c(shift, 0, 0))
  arr <- array(0, c(5, 5, 5))
  arr[2, 3, 3] <- 1
  arr[4, 3, 3] <- 2
  new_atlas(
    "Toy", neuroim2::NeuroVol(arr, sp), ids = 1:2,
    labels = c("A", "B"), hemi = c("left", "right"),
    cmap = matrix(1:6, 2), ref = new_atlas_ref(
      family, "Toy", template_space = "MNI152NLin6Asym",
      coord_space = "MNI152", resolution = "incorrect", confidence = "high"),
    artifacts = .new_atlas_artifact("labels", family, "Toy"),
    history = .new_atlas_history("load", "volume"))
}

test_that("metadata reflect geometry and survive serialization offline", {
  a <- make_toy_metadata_atlas()
  m <- atlas_metadata(a)
  expect_s3_class(m, "NeuroResourceMetadata")
  expect_identical(m$schema_version, 1L)
  expect_equal(m$spatial$voxel_size, c(1, 1, 1))
  expect_equal(atlas_ref(a)$resolution, "1mm")
  expect_equal(m$content$regions, 2)
  expect_equal(m$content$value_type, "labels")
  expect_true(is.na(m$identity$version))
  expect_identical(atlas_metadata(unserialize(serialize(a, NULL))), m)
  expect_identical(atlas_provenance(a)$artifacts, m$artifacts)
  expect_identical(atlas_history(a), m$history)
  a$atlas_ref$resolution <- "stale compatibility copy"
  expect_equal(atlas_ref(a)$resolution, "1mm")
  expect_error(validate_resource_metadata(within(m, schema_version <- 2L)),
               class = "neuroatlas_error_invalid_metadata")
})

test_that("citations distinguish atlas and distribution and export offline", {
  a <- make_toy_metadata_atlas("glasser")
  a$atlas_artifacts$citation_doi <- "10.6084/m9.figshare.3498446"
  a <- .attach_atlas_provenance(a, a$atlas_artifacts, a$atlas_history)
  refs <- atlas_metadata(a)$citations
  expect_true("10.1038/nature18933" %in% refs$doi[refs$role == "atlas"])
  expect_true("10.6084/m9.figshare.3498446" %in%
                refs$doi[refs$role == "distribution"])
  expect_s3_class(atlas_citations(a), "bibentry")
  expect_length(atlas_citations(a, role = "atlas"), 1)
  expect_match(paste(utils::toBibtex(atlas_citations(a)), collapse = "\n"),
               "nature18933")
  expect_match(paste(utils::toBibtex(atlas_citations(a)), collapse = "\n"),
               "@Article\\{neuroatlas_10_1038_nature18933,")
  expect_length(atlas_citations(a, role = "absent"), 0)
  expect_equal(new_atlas_ref("custom", "Custom")$confidence, "uncertain")
})

test_that("artifact receipts hash the bytes actually supplied", {
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(charToRaw("abc"), path)
  art <- .new_atlas_artifact("labels", "toy", "Toy", local_path = path)
  if ("sha256sum" %in% getNamespaceExports("tools")) {
    expect_equal(art$sha256,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    expect_equal(art$checksum_algorithm, "sha256")
  } else {
    expect_equal(art$checksum, "900150983cd24fb0d6963f7d28e17f72")
    expect_equal(art$checksum_algorithm, "md5")
  }
  writeBin(charToRaw("changed"), path)
  expect_false(identical(.file_receipt(path)$checksum, art$checksum))
  expect_true(is.na(.new_atlas_artifact("labels", "toy", "Toy")$license))
})

test_that("subset, dilation and merge preserve their sources and record changes", {
  a <- make_toy_metadata_atlas()
  left <- filter_atlas(a, hemi == "left")
  expect_identical(atlas_artifacts(left), atlas_artifacts(a))
  expect_equal(atlas_metadata(left)$content$regions, 1)
  expect_equal(tail(atlas_history(left)$parameters, 1)[[1]]$keep_ids, 1L)
  mask <- neuroim2::NeuroVol(array(1, c(5, 5, 5)), neuroim2::space(a$atlas))
  d <- dilate_atlas(a, mask, radius = 1, maxn = 5)
  expect_equal(tail(atlas_history(d)$action, 1), "dilate")
  expect_equal(tail(atlas_history(d)$parameters, 1)[[1]]$radius, 1)
  expect_identical(atlas_artifacts(d), atlas_artifacts(a))
  expect_gt(sum(as.vector(neuroim2::as.dense(d$atlas)) != 0), 2)
  b <- make_toy_metadata_atlas("glasser")
  combined <- suppressWarnings(merge_atlases(a, b))
  m <- atlas_metadata(combined)
  expect_length(m$parents, 2)
  expect_identical(m$parents$atlas1, atlas_metadata(a))
  expect_true(all(c("10.1093/cercor/bhx179", "10.1038/nature18933") %in%
                    m$citations$doi))
  expect_equal(tail(m$history$action, 1), "merge")
  expect_equal(m$history$parameters[[1]]$atlas2_id_map, c(`1` = 3L, `2` = 4L))
  expect_error(merge_atlases(a, make_toy_metadata_atlas(shift = 1)),
               "different voxel grids")
  b$metadata$spatial$template_space <- "MNI152NLin2009cAsym"
  expect_error(merge_atlases(a, b), "conflicting anatomical")
})

test_that("resampling reports actual spacing and retains native identity", {
  a <- get_aseg_atlas()
  out <- neuroim2::NeuroSpace(
    as.integer(ceiling(dim(a$atlas) / 2)), spacing = c(2, 2, 2),
    origin = neuroim2::origin(neuroim2::space(a$atlas)))
  b <- get_aseg_atlas(outspace = out)
  expect_equal(atlas_ref(b)$resolution, "2mm")
  expect_equal(atlas_metadata(b)$spatial$voxel_size, c(2, 2, 2))
  expect_equal(atlas_space(b), atlas_space(a))
  expect_equal(atlas_artifacts(b)$resolution, "1mm")
  expect_equal(tail(atlas_history(b)$parameters, 1)[[1]]$interpolation, "nearest")
  expect_equal(atlas_metadata(a)$spatial$basis, "inferred")
  expect_equal(atlas_ref(a)$confidence, "uncertain")
})

test_that("template metadata preserve query keys, references and S4 classes", {
  a <- make_toy_metadata_atlas()
  description <- list(Name = "Synthetic template", Species = "Homo sapiens",
                       TemplateFlowVersion = "1.0.0",
                       License = "CC0", ReferencesAndLinks = c(
                         "https://doi.org/10.1234/example", "Text reference"))
  t <- .attach_template_metadata(
    a$atlas, "not-downloaded.nii.gz", "MNI152NLin6Asym",
    list(resolution = "01", suffix = "T1w"), description = description)
  expect_identical(class(t), class(a$atlas))
  expect_true(methods::validObject(t))
  m <- template_metadata(t)
  expect_equal(m$content$parameters$resolution, "01")
  expect_equal(m$spatial$voxel_size, c(1, 1, 1))
  expect_equal(m$artifacts$license, "CC0")
  expect_true(is.na(m$identity$version))
  expect_equal(m$provenance$templateflow_version, "1.0.0")
  expect_equal(sum(m$citations$role == "template"), 2)
  expect_s3_class(template_citations(t), "bibentry")
  expect_identical(template_metadata(unserialize(serialize(t, NULL))), m)
  expect_error(template_metadata(a$atlas), "No template metadata")
})

test_that("get_template attaches metadata from the resolved file", {
  a <- make_toy_metadata_atlas()
  testthat::local_mocked_bindings(
    .ensure_templateflow = function(...) TRUE,
    .validate_template_space = function(...) TRUE,
    .validate_resolution = function(...) TRUE,
    as_neurovol = function(path) a$atlas,
    .template_description = function(template) list(Name = "Synthetic"))
  testthat::local_mocked_bindings(
    tf_get = function(...) "resolved-template.nii.gz", .package = "templateflow")
  t <- get_template("MNI152NLin6Asym", resolution = "02")
  expect_equal(template_metadata(t)$artifacts$file_name,
               "resolved-template.nii.gz")
  expect_identical(get_template("MNI152NLin6Asym", path_only = TRUE),
                   "resolved-template.nii.gz")
})

test_that("legacy metadata are conservative and catalog coverage is complete", {
  a <- make_toy_metadata_atlas()
  a$metadata <- a$atlas_ref <- a$atlas_artifacts <- a$atlas_history <- NULL
  expect_equal(atlas_metadata(a)$spatial$basis, "unknown")
  expect_equal(atlas_ref(a)$confidence, "uncertain")
  expect_equal(nrow(atlas_metadata(a)$history), 0)
  for (family in setdiff(unique(list_atlases()$family), "subcortical")) {
    expect_true(is.list(.atlas_catalog_entry(family)), info = family)
  }
  for (id in subcortical_atlas_options()$id) {
    expect_gt(nrow(.atlas_catalog_entry("subcortical", id)$citations), 0)
  }
})

make_toy_metadata_surface <- function(hemi, codes) {
  geometry <- neurosurf::SurfaceGeometry(
    vert = rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1)),
    faces = rbind(c(0L, 1L, 2L), c(0L, 2L, 3L)), hemi = hemi)
  methods::new("LabeledNeuroSurface", geometry = geometry,
                indices = 1:4, data = as.numeric(codes),
                labels = c("A", "B"), cols = c("#FF0000", "#0000FF"))
}

test_that("surface metadata describe mesh without enabling unsupported operations", {
  left <- make_toy_metadata_surface("left", c(0, 1, 1, 2))
  right <- make_toy_metadata_surface("right", c(0, 3, 3, 4))
  a <- new_surfatlas(
    name = "Toy surface", lh_atlas = left, rh_atlas = right, ids = 1:4,
    labels = c("A", "B", "C", "D"), hemi = rep(c("left", "right"), each = 2),
    surf_type = "pial", surface_space = "fsaverage", ref = new_atlas_ref(
      "schaefer", "Toy", "surface", template_space = "fsaverage", density = "toy"))
  m <- atlas_metadata(a)
  expect_equal(m$spatial$vertex_count, c(left = 4L, right = 4L))
  expect_equal(m$spatial$surface_type, "pial")
  expect_length(m$spatial$voxel_size, 0)
  expect_error(filter_atlas(a, id %in% c(1L, 3L)),
               class = "neuroatlas_error_unsupported")
  expect_identical(atlas_metadata(unserialize(serialize(a, NULL))), m)
})

test_that("load_surface_template attaches individual and paired records", {
  geom <- make_toy_metadata_surface("left", c(0, 1, 1, 2))@geometry
  testthat::local_mocked_bindings(
    get_surface_template = function(..., hemi) paste0(hemi, ".surf.gii"),
    .read_surface_template_geometry = function(...) geom,
    .template_description = function(template) list(Name = "Toy template"))
  both <- load_surface_template("fsaverage", "pial", hemi = "both", density = "toy")
  expect_s3_class(both, "neuroatlas_surface_pair")
  expect_s4_class(both$L, "SurfaceGeometry")
  expect_equal(template_metadata(both)$spatial$vertex_count, c(L = 4L, R = 4L))
  expect_equal(template_metadata(both$R)$spatial$hemisphere, "R")
  expect_equal(template_metadata(both)$artifacts$file_name,
               c("L.surf.gii", "R.surf.gii"))
  expect_identical(template_metadata(unserialize(serialize(both, NULL))),
                   template_metadata(both))
})

test_that("specialized print methods display the shared spatial contract", {
  a <- make_toy_metadata_atlas()
  for (cl in c("schaefer", "glasser", "atlas")) {
    class(a) <- unique(c(cl, "atlas"))
    output <- paste(capture.output(print(a)), collapse = "\n")
    expect_match(output, "MNI152NLin6Asym")
    expect_match(output, "Voxel size:")
    expect_match(output, "Coord. space:")
    expect_match(output, "Citation:")
    expect_match(output, "Modifications:")
  }
  a$metadata$artifacts <- dplyr::bind_rows(
    .new_atlas_artifact('labels', 'toy', 'Toy', license = 'MIT'),
    .new_atlas_artifact('geometry', 'toy', 'Toy'))
  expect_match(paste(capture.output(print(a)), collapse = '\n'),
               'MIT, 1 artifact\\(s\\) not recorded')
})

test_that("schema errors are explicit for malformed nested fields", {
  original <- atlas_metadata(make_toy_metadata_atlas())
  cases <- list(c("kind"), c("spatial", "basis"),
                c("content", "representation"), c("content", "regions"),
                c("history", "parameters"))
  for (path in cases) {
    invalid <- original
    invalid[[path]] <- NULL
    expect_error(validate_resource_metadata(invalid),
                 class = "neuroatlas_error_invalid_metadata")
  }
})

test_that("operations read authoritative history and upgrade legacy objects", {
  a <- make_toy_metadata_atlas()
  a$atlas_history <- .empty_atlas_history()
  b <- filter_atlas(a, hemi == "left")
  expect_equal(atlas_history(b)$action, c("load", "subset"))
  a$metadata <- NULL
  b <- filter_atlas(a, hemi == "left")
  expect_s3_class(b$metadata, "NeuroResourceMetadata")
  expect_equal(b$metadata$history$action, "subset")
  expect_match(b$metadata$provenance$issues[[1]], "Legacy object")
})

test_that("bundled source receipts remain distinguishable after installation", {
  receipt <- .bundled_file_receipt("olsen_mtl.rda")
  expect_equal(receipt$checksum_basis, "source_manifest")
  expect_equal(receipt$checksum_algorithm, "sha256")
  expect_true(is.na(receipt$local_path))
  expect_match(receipt$checksum, "^[a-f0-9]{64}$")
  path <- system.file("data", "olsen_mtl.rda", package = "neuroatlas")
  if (nzchar(path) && "sha256sum" %in% getNamespaceExports("tools")) {
    expect_equal(receipt$checksum, unname(tools::sha256sum(path)))
  }
  expect_s3_class(atlas_metadata(get_olsen_mtl()), "NeuroResourceMetadata")
})

test_that("Wang probability maps capture actual local files and values", {
  root <- tempfile("wang-metadata-")
  dir.create(file.path(root, "subj_vol_all"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
  path <- file.path(root, "subj_vol_all", "perc_VTPM_vol_roi1_lh.nii.gz")
  vol <- neuroim2::NeuroVol(array(0.25, c(4, 4, 4)), sp)
  neuroim2::write_vol(vol, path)
  result <- get_wang_prob_atlas(prob_dir = root, image = "probability",
    hemi = "lh", rois = "V1v", path_only = FALSE, use_cache = FALSE)
  meta <- atlas_metadata(result)
  expect_equal(meta$content$value_type, "probability")
  expect_equal(meta$content$regions, 1L)
  expect_equal(meta$spatial$voxel_size, c(2, 2, 2))
  expect_equal(meta$artifacts$checksum, .file_receipt(path)$checksum)
  expect_equal(meta$artifacts$source_name, "local_files")
  expect_true(is.na(meta$artifacts$source_version))
  expect_equal(atlas_metadata(result$volumes[[1]]), meta$parents[[1]])
  expect_equal(as.vector(result$volumes[[1]]), rep(0.25, 64))
  expect_s3_class(atlas_citations(result), "bibentry")
})

test_that("TemplateFlow atlas loaders retain source receipts and variant choices", {
  toy <- make_toy_metadata_atlas()
  label_path <- tempfile(fileext = '.tsv')
  on.exit(unlink(label_path), add = TRUE)
  writeLines(c('index\tname', '1\tLeft A', '2\tRight B'), label_path)
  testthat::local_mocked_bindings(
    .ensure_templateflow = function(...) TRUE,
    get_template = function(space, suffix, path_only = FALSE, ...) {
      if (path_only) return(label_path)
      .attach_template_metadata(toy$atlas, 'resolved-dseg.nii.gz', space,
        query = c(list(suffix = suffix), list(...)),
        description = list(Name = 'Synthetic', License = 'CC0'))
    })
  loaders <- list(
    harvard_oxford = function() get_harvard_oxford_atlas(threshold = 50),
    cit168 = function() get_subcortical_atlas('cit168'))
  for (load in loaders) {
    a <- load()
    m <- atlas_metadata(a)
    expect_equal(m$spatial$voxel_size, c(1, 1, 1))
    expect_equal(m$content$regions, 2)
    expect_s3_class(m$parents$source_template, 'NeuroResourceMetadata')
    image <- m$artifacts[m$artifacts$file_name == 'resolved-dseg.nii.gz', ]
    expect_equal(nrow(image), 1)
    expect_true(image$role %in% c('summary_label_volume', 'parcellation_volume'))
    expect_true(is.na(image$license))
    expect_equal(m$artifacts$checksum[m$artifacts$role == 'label_table'],
                 .file_receipt(label_path)$checksum)
    expect_true(all(c('atlas', 'software') %in% m$citations$role))
  }
  expect_equal(atlas_metadata(loaders$harvard_oxford())$content$parameters$threshold,
               50)
})

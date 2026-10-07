make_toy_cifti_file <- function(
  directory, kind = "scalar", brain_axis = 1L,
  xml = NULL, values = NULL
) {
  skip_if_not_installed("RNifti")
  skip_if_not_installed("xml2")
  brain <- paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="', brain_axis,
    '" IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS">',
    '<BrainModel IndexOffset="0" IndexCount="3" ',
    'ModelType="CIFTI_MODEL_TYPE_SURFACE" ',
    'BrainStructure="CIFTI_STRUCTURE_CORTEX_RIGHT" ',
    'SurfaceNumberOfVertices="6"><VertexIndices>3 0 5</VertexIndices>',
    '</BrainModel><Volume VolumeDimensions="2,1,2">',
    '<TransformationMatrixVoxelIndicesIJKtoXYZ MeterExponent="-3">',
    "2 0 0 10 0 -3 0 20 0 0 4 -30 0 0 0 1",
    "</TransformationMatrixVoxelIndicesIJKtoXYZ></Volume>",
    '<BrainModel IndexOffset="3" IndexCount="2" ',
    'ModelType="CIFTI_MODEL_TYPE_VOXELS" ',
    'BrainStructure="CIFTI_STRUCTURE_THALAMUS_LEFT">',
    "<VoxelIndicesIJK>0 0 0 1 0 1</VoxelIndicesIJK></BrainModel>",
    '<BrainModel IndexOffset="5" IndexCount="2" ',
    'ModelType="CIFTI_MODEL_TYPE_SURFACE" ',
    'BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT" ',
    'SurfaceNumberOfVertices="6"><VertexIndices>4 1</VertexIndices>',
    "</BrainModel></MatrixIndicesMap>"
  )
  label <- function(key) {
    paste0(
      '<LabelTable><Label Key="0" Red="0" Green="0" Blue="0" ',
      'Alpha="0">unassigned</Label><Label Key="-3" Red="1" Green="0" ',
      'Blue="0" Alpha="1">negative &amp; shared</Label><Label Key="', key,
      '" Red="0" Green="0.25" Blue="1" Alpha="0.5">map-specific',
      "</Label></LabelTable>"
    )
  }
  maps <- paste0(
    '<MatrixIndicesMap AppliesToMatrixDimension="', 1L - brain_axis,
    '" IndicesMapToDataType="CIFTI_INDEX_TYPE_',
    if (kind == "label") "LABELS" else "SCALARS", '">',
    "<NamedMap><MapName>first &amp; map</MapName><MetaData><MD>",
    "<Name>notes</Name><Value>preserve me</Value></MD></MetaData>",
    if (kind == "label") label(2) else "", "</NamedMap>",
    "<NamedMap><MapName>second map</MapName>",
    if (kind == "label") label(7) else "", "</NamedMap></MatrixIndicesMap>"
  )
  if (is.null(xml)) {
    xml <- paste0(
      '<CIFTI Version="2.0"><Matrix><MetaData><MD><Name>author</Name>',
      "<Value>synthetic test</Value></MD></MetaData>", brain, maps,
      "</Matrix></CIFTI>"
    )
  }
  if (is.null(values)) {
    values <- if (kind == "label") {
      cbind(c(0, -3, 2, -3, 0, 2, 0), c(7, 0, -3, 0, 7, -3, 7))
    } else {
      matrix(c(11, 12, 13, -100, 200, 21, 22, 31, 32, 33, 400, -500, 41, 42),
        nrow = 7L
      )
    }
  }
  stored <- if (brain_axis == 1L) t(values) else values
  image <- RNifti::updateNifti(array(stored, c(rep(1L, 4), dim(stored))),
    template = list(intent_code = if (kind == "label") 3007L else 3006L),
    datatype = "float64"
  )
  RNifti::extension(image, 32) <- charToRaw(xml)
  RNifti::extension(image, 6) <- charToRaw("extra extension preserved")
  file <- tempfile(tmpdir = directory, fileext = paste0(".d", kind, ".nii"))
  RNifti::writeNifti(image, file, version = 2, datatype = "float64")
  list(file = file, values = values, xml = xml, image = image)
}

test_that("CIFTI scalar and label maps preserve both matrix layouts", {
  directory <- withr::local_tempdir()
  for (kind in c("scalar", "label")) {
    for (axis in 0:1) {
      toy <- make_toy_cifti_file(directory, kind, axis)
      x <- read_cifti(toy$file)
      expect_s3_class(x, "CiftiData")
      expect_equal(x$values, toy$values)
      expect_identical(x$brain_axis, axis)
      expect_equal(x$brain_models[[1]]$indices, c(3, 0, 5))
      expect_equal(
        x$brain_models[[2]]$indices,
        matrix(c(0, 0, 0, 1, 0, 1), ncol = 3L, byrow = TRUE)
      )
      expect_equal(x$brain_models[[3]]$indices, c(4, 1))
      expect_equal(
        x$volume$affine_mm,
        matrix(c(2, 0, 0, 10, 0, -3, 0, 20, 0, 0, 4, -30, 0, 0, 0, 1),
          4L,
          byrow = TRUE
        )
      )
      expect_identical(x$map_names, c("first & map", "second map"))
      if (kind == "label") {
        expect_equal(x$label_tables[[1]]$key, c(0, -3, 2))
        expect_equal(x$label_tables[[2]]$key, c(0, -3, 7))
        expect_equal(x$label_tables[[1]]$name[2], "negative & shared")
        expect_equal(x$label_tables[[2]]$alpha, c(0, 1, 0.5))
      }
      file <- tempfile(tmpdir = directory, fileext = ".nii")
      expect_invisible(write_cifti(x, file))
      y <- read_cifti(file)
      expect_identical(y$xml, x$xml)
      expect_identical(y$extensions, x$extensions)
      expect_identical(y$brain_models, x$brain_models)
      expect_identical(y$label_tables, x$label_tables)
      expect_equal(y$values, x$values)
      expect_identical(y$brain_axis, axis)
      expect_error(write_cifti(x, file), "Destination exists")
      expect_invisible(write_cifti(x, file, overwrite = TRUE))
    }
  }
})

test_that("CIFTI single-map writing retains both matrix axes", {
  directory <- withr::local_tempdir()
  for (axis in 0:1) {
    toy <- make_toy_cifti_file(directory, brain_axis = axis)
    doc <- xml2::read_xml(toy$xml)
    maps <- xml2::xml_find_all(doc, "./Matrix/MatrixIndicesMap/NamedMap")
    xml2::xml_remove(maps[[2]])
    single <- make_toy_cifti_file(directory, brain_axis = axis,
      xml = as.character(doc), values = toy$values[, 1, drop = FALSE]
    )
    source <- read_cifti(single$file)
    file <- tempfile(tmpdir = directory, fileext = ".nii")
    write_cifti(source, file)
    connection <- base::file(file, "rb")
    bytes <- readBin(connection, "raw", 24L)
    close(connection)
    expected <- as.raw(c(6, rep(0, 7)))
    if (identical(bytes[1:4], as.raw(c(0, 0, 2, 28)))) {
      expected <- rev(expected)
    }
    expect_identical(bytes[17:24], expected)
    expect_identical(read_cifti(file)$values, source$values)
  }
})

test_that("CIFTI replacement validates dimensions, values and immutable metadata", {
  directory <- withr::local_tempdir()
  toy <- make_toy_cifti_file(directory)
  x <- read_cifti(toy$file)
  values <- x$values * 3
  values[4, 1] <- NA_real_
  y <- replace_cifti_values(x, values)
  expect_equal(y$values, values)
  expect_identical(y$provenance$input_id, x$id)
  expect_false(identical(y$id, x$id))
  expect_identical(y$brain_models, x$brain_models)
  expect_identical(x$values, toy$values)
  file <- file.path(directory, "changed.dscalar.nii")
  write_cifti(y, file)
  expect_equal(read_cifti(file)$values, values)
  expect_error(replace_cifti_values(x, values[-1, ]))
  values[1, 1] <- Inf
  expect_error(replace_cifti_values(x, values))
  bad <- x
  bad$brain_models[[1]]$indices <- c(0, 3, 5)
  expect_error(write_cifti(bad, file, overwrite = TRUE), "modified CiftiData")
  labels <- read_cifti(make_toy_cifti_file(directory, "label")$file)
  values <- labels$values
  values[1, 2] <- 2
  expect_error(replace_cifti_values(labels, values), "label table")
  values[1, 2] <- NA_real_
  expect_error(replace_cifti_values(labels, values), "missing labels")
})

test_that("CIFTI value operations report missing optional XML support", {
  check <- .require_cifti_io
  environment(check) <- list2env(
    list(requireNamespace = function(package, ...) package != "xml2"),
    parent = environment(.require_cifti_io)
  )
  for (operation in list(replace_cifti_values, apply_cifti_transform)) {
    environment(operation) <- list2env(
      list(.require_cifti_io = check), parent = environment(operation)
    )
    expect_error(operation(NULL, NULL), "optional 'xml2'")
  }
})

test_that("CIFTI rejects corrupt brain models and unsupported mappings", {
  directory <- withr::local_tempdir()
  toy <- make_toy_cifti_file(directory)
  cases <- list(
    gap = c('IndexOffset="3"', 'IndexOffset="4"', "ranges"),
    duplicate = c("3 0 5", "3 0 3", "Duplicate"),
    out_of_range = c("3 0 5", "3 0 6", "outside"),
    voxel = c("0 0 0 1 0 1", "0 0 0 2 0 1", "in-range"),
    vertex_count = c(
      'SurfaceNumberOfVertices="6"',
      'SurfaceNumberOfVertices="3"', "outside"
    ),
    wrong_count = c('IndexCount="3"', 'IndexCount="2"', "count"),
    axes = c(
      'AppliesToMatrixDimension="0"',
      'AppliesToMatrixDimension="1"', "matrix axes"
    ),
    series = c("CIFTI_INDEX_TYPE_SCALARS", "CIFTI_INDEX_TYPE_SERIES", "supported"),
    version = c('Version="2.0"', 'Version="1.0"', "CIFTI-2"),
    affine = c("2 0 0 10", "0 0 0 10", "affine")
  )
  for (case in cases) {
    xml <- sub(case[1], case[2], toy$xml, fixed = TRUE)
    invalid <- make_toy_cifti_file(directory, xml = xml)
    expect_error(read_cifti(invalid$file), case[3])
  }
  xml <- sub("<CIFTI", '<!DOCTYPE CIFTI [<!ENTITY local "bad">]><CIFTI',
    toy$xml,
    fixed = TRUE
  )
  invalid <- make_toy_cifti_file(directory, xml = xml)
  expect_error(read_cifti(invalid$file), "entity declarations")
  header <- RNifti::niftiHeader(RNifti::readNifti(toy$file))
  header$intent_code <- 3007L
  expect_error(.cifti_parse(toy$xml, header), "intent")
})

test_that("CIFTI labels reject invalid keys and color values", {
  directory <- withr::local_tempdir()
  toy <- make_toy_cifti_file(directory, "label")
  for (replacement in c('Key="2.5"', 'Key="0"', 'Key="99"')) {
    xml <- sub('Key="2"', replacement, toy$xml, fixed = TRUE)
    invalid <- make_toy_cifti_file(directory, "label", xml = xml)
    expect_error(read_cifti(invalid$file), "CIFTI|label table")
  }
  xml <- sub('Green="0.25"', 'Green="1.5"', toy$xml, fixed = TRUE)
  invalid <- make_toy_cifti_file(directory, "label", xml = xml)
  expect_error(read_cifti(invalid$file), "RGBA")
})

make_toy_cifti_operators <- function(directory) {
  skip_if_not_installed("neurotransform")
  if (!.surface_engine_revision_verified(
    "933edddda462593941e167726e8aaa7168ff103a"
  )) {
    skip("Qualified pinned surface engine is unavailable")
  }
  sphere <- rbind(diag(3), -diag(3))
  triangles <- rbind(
    c(0, 1, 2), c(0, 2, 4), c(0, 4, 5), c(0, 5, 1),
    c(3, 2, 1), c(3, 4, 2), c(3, 5, 4), c(3, 1, 5)
  )
  operators <- lapply(c("L", "R"), function(hemi) {
    domain <- surface_domain(
      "toy", hemi, "6v", sphere, triangles,
      rep(TRUE, 6), "analytic", "v1"
    )
    geometry <- surface_geometry(domain, sphere, triangles, rep(TRUE, 6))
    get_template_transform(geometry, geometry,
      cache_dir = file.path(directory, "operators")
    )
  })
  names(operators) <- c("L", "R")
  operators
}

test_that("CIFTI cortical adapters preserve subcortex through reordered layouts", {
  directory <- withr::local_tempdir()
  operators <- make_toy_cifti_operators(directory)
  for (kind in c("scalar", "label")) {
    toy <- make_toy_cifti_file(directory, kind)
    from <- read_cifti(toy$file)
    xml <- sub("3 0 5", "5 3 0", toy$xml, fixed = TRUE)
    xml <- sub("0 0 0 1 0 1", "1 0 1 0 0 0", xml, fixed = TRUE)
    xml <- sub("4 1</VertexIndices>", "1 4</VertexIndices>", xml, fixed = TRUE)
    xml <- sub('Dimension="1"', 'Dimension="B"', xml, fixed = TRUE)
    xml <- sub('Dimension="0"', 'Dimension="1"', xml, fixed = TRUE)
    xml <- sub('Dimension="B"', 'Dimension="0"', xml, fixed = TRUE)
    to <- read_cifti(make_toy_cifti_file(directory, kind, 0L, xml)$file)
    transform <- get_template_transform(from, to,
      cortex = operators, volume_space = "MNI152NLin6Asym",
      cache_dir = file.path(directory, "cache")
    )
    expect_s3_class(transform, "CiftiTransform")
    mapped <- apply_template_transform(from, transform)
    expect_equal(mapped$values, from$values[c(3, 1, 2, 5, 4, 7, 6), ])
    expect_true(all(mapped$available))
    expect_identical(mapped$brain_models, to$brain_models)
    expect_identical(mapped$volume, from$volume)
    expect_identical(mapped$map_names, from$map_names)
    expect_identical(mapped$label_tables, from$label_tables)
    expect_identical(mapped$brain_axis, from$brain_axis)
    expect_match(mapped$xml, "preserve me")
    file <- tempfile(tmpdir = directory, fileext = ".nii")
    write_cifti(mapped, file)
    restored <- read_cifti(file)
    expect_equal(restored$values, mapped$values)
    expect_identical(restored$available, mapped$available)
    wrong <- transform
    wrong$copy_rows[4] <- 1L
    expect_error(apply_cifti_transform(from, wrong), "modified CiftiTransform")
    expect_error(
      apply_template_transform(from, transform, data_type = "probability"),
      "declared map semantics"
    )
  }
})

test_that("CIFTI adapters reject support loss and ambiguous domain bindings", {
  directory <- withr::local_tempdir()
  operators <- make_toy_cifti_operators(directory)
  toy <- make_toy_cifti_file(directory)
  from <- read_cifti(toy$file)
  expect_error(get_cifti_transform(from, from, operators), "volume_space")
  expect_error(get_cifti_transform(
    from, from, operators,
    c(from = "MNI152NLin6Asym", to = "MNI152NLin2009cAsym")
  ), "unchanged exact volume")
  expect_error(get_cifti_transform(from, from, operators["L"], "MNI152NLin6Asym"), "Missing")
  expect_error(get_cifti_transform(from, from, operators[c("R", "L")], "MNI152NLin6Asym"), NA)
  wrong <- list(L = operators$R, R = operators$L)
  expect_error(get_cifti_transform(from, from, wrong, "MNI152NLin6Asym"), "explicit operator binding")
  for (case in list(
    c("0 0 0 1 0 1", "0 0 0 1 0 0", "index support"),
    c("2 0 0 10", "2 0 0 11", "volume geometry"),
    c(
      "CIFTI_STRUCTURE_THALAMUS_LEFT", "CIFTI_STRUCTURE_THALAMUS_RIGHT",
      "brain structures"
    )
  )) {
    xml <- sub(case[1], case[2], toy$xml, fixed = TRUE)
    to <- read_cifti(make_toy_cifti_file(directory, xml = xml)$file)
    expect_error(get_cifti_transform(from, to, operators, "MNI152NLin6Asym"), case[3])
  }
})

test_that("missing CIFTI cortical labels require explicit keys and retain availability", {
  directory <- withr::local_tempdir()
  operators <- make_toy_cifti_operators(directory)
  toy <- make_toy_cifti_file(directory, "label")
  from <- read_cifti(toy$file)
  doc <- xml2::read_xml(toy$xml)
  models <- xml2::xml_find_all(doc, "./Matrix/MatrixIndicesMap/BrainModel")
  xml2::xml_set_attr(models[[1]], "IndexCount", 5)
  xml2::xml_set_text(xml2::xml_find_first(models[[1]], "./VertexIndices"), "3 0 5 2 4")
  xml2::xml_set_attr(models[[2]], "IndexOffset", 5)
  xml2::xml_set_attr(models[[3]], "IndexOffset", 7)
  target_file <- make_toy_cifti_file(directory, "label",
    xml = as.character(doc), values = matrix(0, 9, 2)
  )$file
  to <- read_cifti(target_file)
  transform <- get_cifti_transform(from, to, operators, "MNI152NLin6Asym")
  expect_error(apply_cifti_transform(from, transform), "explicit missing_labels")
  expect_error(
    apply_cifti_transform(from, transform, missing_labels = c(2, 2)),
    "that map's label table"
  )
  mapped <- apply_cifti_transform(from, transform, missing_labels = c(0, -3))
  expect_equal(mapped$values[4:5, 1], c(0, 0))
  expect_equal(mapped$values[4:5, 2], c(-3, -3))
  expect_false(any(mapped$available[4:5, ]))
  expect_true(all(mapped$available[c(1:3, 6:9), ]))
  expect_equal(mapped$values[6:7, ], from$values[4:5, ])
  file <- file.path(directory, "with-unassigned.dlabel.nii")
  write_cifti(mapped, file)
  restored <- read_cifti(file)
  expect_identical(restored$available, mapped$available)
  expect_equal(restored$values, mapped$values)
  replaced <- replace_cifti_values(restored, restored$values)
  expect_identical(replaced$available, restored$available)
  replaced_file <- file.path(directory, "replaced-unassigned.dlabel.nii")
  write_cifti(replaced, replaced_file)
  expect_identical(read_cifti(replaced_file)$available, restored$available)
  reverse <- get_cifti_transform(restored, restored, operators, "MNI152NLin6Asym")
  expect_error(apply_cifti_transform(restored, reverse), "explicit missing_labels")
  expect_error(apply_cifti_transform(replaced, reverse), "explicit missing_labels")
})

test_that("FSL XML parser reads image entries and labels", {
  atlas_dir <- file.path(tempdir(), "fsl-atlas-test")
  dir.create(file.path(atlas_dir, "HarvardOxford"), recursive = TRUE,
             showWarnings = FALSE)
  img <- file.path(atlas_dir, "HarvardOxford", "toy-prob-1mm.nii.gz")
  summary <- file.path(atlas_dir, "HarvardOxford", "toy-maxprob-1mm.nii.gz")
  file.create(img)
  file.create(summary)

  xml <- file.path(atlas_dir, "Toy.xml")
  writeLines(c(
    "<atlas>",
    "  <header>",
    "    <name>Toy Probabilistic Atlas</name>",
    "    <shortname>TOY</shortname>",
    "    <type>Probabilistic</type>",
    "    <images>",
    "      <imagefile>/HarvardOxford/toy-prob-1mm</imagefile>",
    "      <summaryimagefile>/HarvardOxford/toy-maxprob-1mm</summaryimagefile>",
    "    </images>",
    "  </header>",
    "  <data>",
    "    <label index=\"0\" x=\"1\" y=\"2\" z=\"3\">Area A</label>",
    "    <label index=\"1\" x=\"4\" y=\"5\" z=\"6\">Area B</label>",
    "  </data>",
    "</atlas>"
  ), xml)

  meta <- .parse_fsl_atlas_xml(xml)
  expect_equal(meta$name, "Toy Probabilistic Atlas")
  expect_equal(meta$shortname, "TOY")
  expect_equal(meta$type, "probabilistic")
  expect_equal(meta$images$resolution, "1mm")
  expect_equal(meta$images$imagefile, normalizePath(img))
  expect_equal(meta$images$summaryimagefile, normalizePath(summary))
  expect_equal(meta$labels$name, c("Area A", "Area B"))
})

test_that("FSL max-probability labels apply documented probabilistic offset", {
  label_tbl <- tibble::tibble(
    index = c(0L, 1L),
    name = c("Area A", "Area B")
  )

  labels <- .labels_for_fsl_values(
    label_tbl,
    values = c(1L, 2L),
    type = "probabilistic",
    image = "summary"
  )

  expect_equal(labels$ids, c(1L, 2L))
  expect_equal(labels$labels, c("Area A", "Area B"))
})

test_that("get_fsl_atlas(path_only=TRUE) resolves local Julich alias", {
  fsl_dir <- file.path(tempdir(), "fake-fsl")
  atlas_dir <- file.path(fsl_dir, "data", "atlases")
  dir.create(file.path(atlas_dir, "Juelich"), recursive = TRUE,
             showWarnings = FALSE)
  img <- file.path(atlas_dir, "Juelich", "Juelich-maxprob-1mm.nii.gz")
  file.create(img)
  xml <- file.path(atlas_dir, "Juelich.xml")
  writeLines(c(
    "<atlas>",
    "  <header>",
    "    <name>Juelich Histological Atlas</name>",
    "    <shortname>Juelich</shortname>",
    "    <type>Probabilistic</type>",
    "    <images>",
    "      <imagefile>/Juelich/Juelich-prob-1mm</imagefile>",
    "      <summaryimagefile>/Juelich/Juelich-maxprob-1mm</summaryimagefile>",
    "    </images>",
    "  </header>",
    "  <data>",
    "    <label index=\"0\">Area 44</label>",
    "  </data>",
    "</atlas>"
  ), xml)

  paths <- get_fsl_atlas("brodmann", fsl_dir = fsl_dir, path_only = TRUE)
  expect_s3_class(paths, "fsl_atlas_paths")
  expect_equal(paths$atlas_path, normalizePath(img))
  expect_equal(paths$labels$name, "Area 44")
})

test_that("Harvard-Oxford TemplateFlow paths resolve when TemplateFlow is available", {
  skip_on_cran()
  skip_if_not_installed("templateflow")

  paths <- tryCatch(
    get_harvard_oxford_atlas(path_only = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(paths), "TemplateFlow Harvard-Oxford files unavailable")

  expect_s3_class(paths, "harvard_oxford_paths")
  expect_true(file.exists(paths$atlas_path))
  expect_true(file.exists(paths$label_path))
  expect_equal(paths$template_atlas, "HOCPA")
})

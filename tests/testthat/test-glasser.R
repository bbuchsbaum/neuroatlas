test_that("Glasser volume defaults to the runtime-stable xcpEngine source", {
  default_sources <- eval(formals(get_glasser_atlas)$source)
  expect_equal(default_sources[[1]], "xcpengine")

  info <- .glasser_volume_source_info("xcpengine")
  expect_match(info$volume_url,
               "^https://raw\\.githubusercontent\\.com/PennLINC/xcpEngine/")
  expect_match(info$label_url,
               "^https://raw\\.githubusercontent\\.com/PennLINC/xcpEngine/")
  expect_false(grepl("PennBBL", info$volume_url))
  expect_match(info$provenance, "PennLINC")
})

test_that("Glasser mni2009c source records pointer-stub risk", {
  info <- .glasser_volume_source_info("mni2009c")
  expect_match(info$volume_url, "Raj-Lab-UCSF")
  expect_match(info$notes, "git-annex pointer")
})

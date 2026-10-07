test_that("neurosurf release numbering satisfies the optional dependency", {
  path <- testthat::test_path("..", "..", "DESCRIPTION")
  skip_if_not(file.exists(path), "source package metadata check")
  description <- read.dcf(path)
  suggests <- trimws(strsplit(description[1, "Suggests"], ",")[[1]])
  requirement <- suggests[grepl("^neurosurf ", suggests)]
  expect_length(requirement, 1L)
  expect_match(requirement, "^neurosurf \\(>= [0-9.]+\\)$")
  minimum <- sub("^neurosurf \\(>= ([0-9.]+)\\)$", "\\1", requirement)
  # Upstream reset 0.1.0.9004 to 0.1.0 during CRAN preparation.
  expect_true(package_version("0.1.0") >= package_version(minimum))

  remotes <- trimws(strsplit(description[1, "Remotes"], ",")[[1]])
  remote <- remotes[grepl("^bbuchsbaum/neurosurf", remotes)]
  expect_length(remote, 1L)
  expect_match(remote, "^bbuchsbaum/neurosurf@[a-f0-9]{40}$")
})

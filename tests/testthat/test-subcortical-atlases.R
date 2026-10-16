test_that(
  "subcortical atlas options expose expected metadata",
  {
    opts <- subcortical_atlas_options()
    expect_true(is.data.frame(opts))
    expect_true(all(c("id", "atlas", "default_space") %in% names(opts)))
    expect_true(
      all(
        c("cit168", "hcp_thalamus", "mdtb10", "hcp_hippamyg") %in%
          opts$id
      )
    )
  }
)

test_that(
  "aliases resolve to canonical subcortical atlas ids",
  {
    spec <- neuroatlas:::`.match_subcortical_spec`("Thalamus_HCP")
    expect_equal(spec$id, "hcp_thalamus")
  }
)

test_that(
  "invalid subcortical atlas names error clearly",
  {
    expect_error(
      get_subcortical_atlas("not_real_atlas"),
      "Unknown subcortical atlas"
    )
  }
)

test_that(
  "get_aseg_atlas() assigns every Left-/Right- structure a hemisphere (#57)",
  {
    a <- get_aseg_atlas()
    hemi <- stats::setNames(a$hemi, a$ids)
    expect_identical(unname(hemi["28"]), "left") # Left-VentralDC
    expect_identical(unname(hemi["60"]), "right") # Right-VentralDC
    # Only the midline Brainstem (16) lacks a hemisphere.
    expect_identical(as.integer(names(hemi)[is.na(hemi)]), 16L)
    expect_identical(sum(hemi == "left", na.rm = TRUE), 8L)
    expect_identical(sum(hemi == "right", na.rm = TRUE), 8L)
    left_ids <- a$ids[which(a$hemi == "left")]
    expect_true(all(left_ids < 40L))
    expect_true(all(a$ids[which(a$hemi == "right")] >= 40L))
    # Each bilateral label appears once per hemisphere.
    bilateral <- setdiff(unique(a$labels), "Brainstem")
    for (lab in bilateral) {
      expect_setequal(a$hemi[a$labels == lab], c("left", "right"))
    }
    expect_length(filter_atlas(a, hemi == "left")$ids, 8L)
  }
)

test_that(
  "registry default_space for aseg matches get_aseg_atlas() (#58)",
  {
    reg <- list_atlases()
    a <- get_aseg_atlas()
    expect_identical(
      reg$default_space[reg$id == "aseg"],
      a$template_space
    )
    expect_identical(a$template_space, "MNI152_unspecified")
  }
)

test_that(
  "get_schaefer_atlas() documents every accepted parcel count (#58)",
  {
    rd <- testthat::test_path("..", "..", "man", "get_schaefer_atlas.Rd")
    skip_if_not(file.exists(rd), "package sources not available")
    txt <- paste(readLines(rd, warn = FALSE), collapse = " ")
    param <- regmatches(txt, regexpr("\\\\item\\{parcels\\}\\{[^}]*\\}", txt))
    accepted <- eval(formals(get_schaefer_atlas)$parcels)
    for (p in accepted) {
      expect_match(param, paste0('"', p, '"'), fixed = TRUE, info = p)
    }
  }
)

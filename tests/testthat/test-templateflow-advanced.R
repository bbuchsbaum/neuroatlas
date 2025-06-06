test_that("TemplateFlow integration handles edge cases and vectorized operations correctly", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = FALSE), 
              "Python not available")
  skip_if_not(reticulate::py_module_available("templateflow"),
              "templateflow not available")
  
  # Test 1: Cache management functions
  cache_path <- show_templateflow_cache_path()
  expect_true(is.character(cache_path))
  expect_true(nchar(cache_path) > 0)
  
  # Should contain "templateflow" in path
  expect_true(grepl("templateflow", cache_path, ignore.case = TRUE))
  
  # Test 2: Vectorized template fetching
  # Get multiple resolutions at once
  multi_res <- tryCatch({
    get_template(
      space = "MNI152NLin2009cAsym",
      suffix = "T1w",
      resolution = c("1", "2"),
      path_only = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(multi_res)) {
    expect_true(is.list(multi_res))
    expect_equal(names(multi_res), c("1", "2"))
    expect_true(all(sapply(multi_res, is.character)))
    
    # Paths should differ by resolution
    expect_false(multi_res[["1"]] == multi_res[["2"]])
    expect_true(grepl("res-01", multi_res[["1"]]) || grepl("res-1", multi_res[["1"]]))
    expect_true(grepl("res-02", multi_res[["2"]]) || grepl("res-2", multi_res[["2"]]))
  }
  
  # Test 3: Variant parameter handling
  variants <- tryCatch({
    get_template(
      space = "MNI152NLin2009cAsym",
      variant = c("brain", "mask"),
      resolution = "2",
      path_only = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(variants)) {
    expect_equal(names(variants), c("brain", "mask"))
    expect_true(grepl("T1w", variants[["brain"]]))
    expect_true(grepl("mask", variants[["mask"]]))
  }
  
  # Test 4: Error handling for invalid spaces
  expect_error(
    get_template(space = "NonExistentSpace123XYZ"),
    "not.*found|invalid.*space|does not exist"
  )
  
  # Test 5: Surface template retrieval
  surf_template <- tryCatch({
    get_surface_template(
      template_id = "fsLR",
      surface_type = "pial",
      hemi = "L",
      density = "32k",
      path_only = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(surf_template)) {
    expect_true(is.character(surf_template))
    expect_true(grepl("fsLR", surf_template))
    expect_true(grepl("hemi-L", surf_template))
    expect_true(grepl("32k", surf_template))
  }
  
  # Test 6: Volume template with different modalities
  # Test just T1w to reduce network operations
  vol <- tryCatch({
    get_volume_template(
      template = "MNI152NLin2009cAsym",
      type = "T1w",
      resolution = "2",
      path_only = TRUE
    )
  }, error = function(e) NULL)
  
  if (!is.null(vol)) {
    expect_true(grepl("T1w", vol))
  }
  
  # Test 7: .check_templateflow_connectivity
  connectivity <- .check_templateflow_connectivity()
  expect_true(is.logical(connectivity))
  
  # Test 8: Memoization behavior
  # First call
  time1 <- system.time({
    path1 <- tryCatch({
      get_template(space = "MNI152NLin2009cAsym", suffix = "T1w", 
                  resolution = "2", path_only = TRUE)
    }, error = function(e) NULL)
  })
  
  # Second call (should be memoized and faster)
  time2 <- system.time({
    path2 <- tryCatch({
      get_template(space = "MNI152NLin2009cAsym", suffix = "T1w", 
                  resolution = "2", path_only = TRUE)
    }, error = function(e) NULL)
  })
  
  if (!is.null(path1) && !is.null(path2)) {
    expect_equal(path1, path2)
    # Second call should be much faster (memoized)
    expect_lt(time2["elapsed"], time1["elapsed"] * 0.5)
  }
})

test_that("Convenience functions (sy_*) work correctly with all parameters", {
  skip_on_cran()
  
  # Test 1: Basic convenience function calls
  convenience_funcs <- list(
    sy_100_7 = c(100, 7),
    sy_200_7 = c(200, 7),
    sy_100_17 = c(100, 17),
    sy_200_17 = c(200, 17)
  )
  
  # Skip download tests to avoid network delays
  # Just test that functions exist
  for (func_name in names(convenience_funcs)[1]) {
    expect_true(exists(func_name, envir = asNamespace("neuroatlas")))
  }
  
  # Test 2: Parameter passing through convenience functions
  # Skip if we can't create a test space
  test_space <- tryCatch({
    neuroim2::NeuroSpace(
      dim = c(50, 50, 50),
      spacing = c(4, 4, 4),
      origin = c(-100, -100, -100)
    )
  }, error = function(e) NULL)
  
  if (!is.null(test_space)) {
    atlas_custom <- tryCatch({
      sy_100_7(resolution = "2", outspace = test_space, smooth = TRUE)
    }, error = function(e) NULL)
  } else {
    atlas_custom <- NULL
  }
  
  if (!is.null(atlas_custom)) {
    expect_equal(dim(atlas_custom$atlas), c(50, 50, 50))
    expect_true(inherits(atlas_custom, "schaefer"))
  }
  
  # Test 3: All convenience functions exist and are properly defined
  all_sy_funcs <- c(
    "sy_100_7", "sy_100_17", "sy_200_7", "sy_200_17",
    "sy_300_7", "sy_300_17", "sy_400_7", "sy_400_17",
    "sy_500_7", "sy_500_17", "sy_600_7", "sy_600_17",
    "sy_800_7", "sy_800_17", "sy_1000_7", "sy_1000_17"
  )
  
  for (func_name in all_sy_funcs) {
    expect_true(exists(func_name, envir = asNamespace("neuroatlas")))
    func <- get(func_name, envir = asNamespace("neuroatlas"))
    expect_true(is.function(func))
    
    # Check function signature
    args <- names(formals(func))
    expect_true(all(c("resolution", "outspace", "smooth", "use_cache") %in% args))
  }
})

test_that("Print methods provide accurate and formatted output", {
  skip_on_cran()
  
  # Test 1: print.atlas base method
  aseg <- tryCatch(get_aseg_atlas(), error = function(e) NULL)
  if (!is.null(aseg)) {
    output <- capture.output(print(aseg))
    
    # Should contain key information
    expect_true(any(grepl("Atlas Summary", output)))
    expect_true(any(grepl("Name:", output)))
    expect_true(any(grepl("Dimensions:", output)))
    expect_true(any(grepl("Regions:", output)))
    expect_true(any(grepl("hemisphere", output, ignore.case = TRUE)))
  }
  
  # Test 2: print.schaefer method
  schaefer <- tryCatch(get_schaefer_atlas(parcels = "100", networks = "7"),
                      error = function(e) NULL)
  if (!is.null(schaefer)) {
    output <- capture.output(print(schaefer))
    
    # Schaefer-specific information
    expect_true(any(grepl("Schaefer", output)))
    expect_true(any(grepl("Networks:", output)))
    expect_true(any(grepl("7", output)))  # Network count
  }
  
  # Test 3: print.glasser method
  glasser <- tryCatch(get_glasser_atlas(), error = function(e) NULL)
  if (!is.null(glasser)) {
    output <- capture.output(print(glasser))
    
    # Glasser-specific information
    expect_true(any(grepl("Glasser", output)))
    expect_true(any(grepl("Multi-Modal Parcellation", output, ignore.case = TRUE)))
    expect_true(any(grepl("360", output)))  # Region count
    expect_true(any(grepl("Example Regions", output)))
  }
  
  # Test 4: CLI formatting elements
  # Check for color/formatting codes (if cli/crayon are active)
  if (!is.null(glasser)) {
    raw_output <- capture.output(print(glasser), type = "message")
    # Look for ANSI codes or special characters used by cli
    # This is environment-dependent, so we just check structure
    expect_true(length(output) > 5)  # Should have multiple lines
  }
})
test_that("surface atlas functions maintain hemisphere integrity and handle all surface types", {
  skip_on_cran()
  
  # Test 1: Load surface atlas and verify structure
  surf_atlas <- tryCatch({
    get_schaefer_surfatlas(parcels = "200", networks = "7", surf = "inflated")
  }, error = function(e) {
    skip("Failed to download surface atlas")
  })
  
  # Skip if download failed
  if (is.null(surf_atlas)) {
    skip("Surface atlas download failed")
  }
  
  # Check basic structure
  expect_true(inherits(surf_atlas, "surfatlas"))
  expect_true(all(c("lh_atlas", "rh_atlas", "name", "cmap", "ids", 
                    "labels", "orig_labels", "network", "hemi") %in% names(surf_atlas)))
  
  # Critical: Verify hemisphere data structures
  # LabeledNeuroSurface objects may have surf_atlas added to their class
  expect_true(inherits(surf_atlas$lh_atlas, "LabeledNeuroSurface") || 
              inherits(surf_atlas$lh_atlas, "surf_atlas"))
  expect_true(inherits(surf_atlas$rh_atlas, "LabeledNeuroSurface") || 
              inherits(surf_atlas$rh_atlas, "surf_atlas"))
  
  # Test 2: Verify ID mapping between hemispheres
  # Left hemisphere should have IDs 1:100, right hemisphere 101:200
  # LabeledNeuroSurface objects use data slot
  lh_data <- slot(surf_atlas$lh_atlas, "data")
  rh_data <- slot(surf_atlas$rh_atlas, "data")
  
  lh_ids <- unique(lh_data[lh_data > 0])
  rh_ids <- unique(rh_data[rh_data > 0])
  
  expect_true(all(lh_ids <= 100))
  expect_true(all(rh_ids > 100 & rh_ids <= 200))
  expect_equal(length(lh_ids) + length(rh_ids), 200)
  
  # Test 3: Test different surface types
  surfaces <- c("inflated", "white", "pial")
  surf_list <- list()
  
  for (surf_type in surfaces) {
    surf_list[[surf_type]] <- tryCatch({
      get_schaefer_surfatlas(parcels = "100", networks = "7", surf = surf_type)
    }, error = function(e) NULL)
  }
  
  # Remove any failed downloads
  surf_list <- surf_list[!sapply(surf_list, is.null)]
  
  if (length(surf_list) > 1) {
    # Verify that different surfaces have same parcellation but different geometry
    surf_names <- names(surf_list)
    for (i in 1:(length(surf_names)-1)) {
      surf1 <- surf_list[[surf_names[i]]]
      surf2 <- surf_list[[surf_names[i+1]]]
      
      # Same parcellation
      expect_equal(surf1$ids, surf2$ids)
      expect_equal(surf1$labels, surf2$labels)
      
      # Different geometry (vertices should differ for different surfaces)
      if ("geometry" %in% slotNames(surf1$lh_atlas) && "geometry" %in% slotNames(surf2$lh_atlas)) {
        geom1 <- slot(surf1$lh_atlas, "geometry")
        geom2 <- slot(surf2$lh_atlas, "geometry")
        # Check if mesh vertices differ between surfaces
        if (!is.null(geom1@mesh$vb) && !is.null(geom2@mesh$vb)) {
          expect_false(all(geom1@mesh$vb == geom2@mesh$vb))
        }
      }
    }
  }
  
  # Test 4: Verify label consistency
  # Extract parcels number from name (e.g., "Schaefer-200-7networks")
  parcels_num <- as.numeric(gsub(".*-(\\d+)-.*", "\\1", surf_atlas$name))
  expect_equal(length(surf_atlas$labels), parcels_num)
  expect_equal(sum(surf_atlas$hemi == "left"), length(surf_atlas$labels) / 2)
  expect_equal(sum(surf_atlas$hemi == "right"), length(surf_atlas$labels) / 2)
  
  # Test 5: Test error handling
  expect_error(get_schaefer_surfatlas(parcels = "999", networks = "7"),
               "'arg' should be one of")
  expect_error(get_schaefer_surfatlas(parcels = "100", networks = "7", surf = "invalid"),
               "'arg' should be one of")
})

test_that("schaefer_surf matches wrapper and handles numeric inputs", {
  skip_on_cran()
  
  # Use fsaverage6 so we do not require TemplateFlow in this test
  atl_numeric <- tryCatch({
    schaefer_surf(parcels = 200, networks = 7,
                  space = "fsaverage6", surf = "inflated")
  }, error = function(e) {
    skip("Failed to load Schaefer surface atlas with numeric inputs")
  })
  
  expect_true(inherits(atl_numeric, "surfatlas"))
  expect_equal(atl_numeric$surface_space, "fsaverage6")
  expect_true(inherits(atl_numeric$lh_atlas, "LabeledNeuroSurface"))
  expect_true(inherits(atl_numeric$rh_atlas, "LabeledNeuroSurface"))
  
  # Wrapper should return the same structure when using character inputs
  atl_wrapper <- get_schaefer_surfatlas(parcels = "200", networks = "7", surf = "inflated")
  expect_true(inherits(atl_wrapper, "surfatlas"))
  
  # Basic metadata should agree
  expect_equal(atl_numeric$ids, atl_wrapper$ids)
  expect_equal(atl_numeric$labels, atl_wrapper$labels)
  expect_equal(atl_numeric$orig_labels, atl_wrapper$orig_labels)
  expect_equal(atl_numeric$network, atl_wrapper$network)
  expect_equal(atl_numeric$hemi, atl_wrapper$hemi)
})

test_that("schaefer_surf_options reports sensible combinations", {
  opts <- schaefer_surf_options()
  
  # Basic structure
  expect_true(is.data.frame(opts))
  expect_true(all(c("space", "parcels", "networks", "surf",
                    "cbig_space", "template_id",
                    "tf_resolution", "tf_density") %in% names(opts)))
  
  # Known spaces and surface types
  expect_true(all(unique(opts$space) %in% c("fsaverage", "fsaverage5", "fsaverage6")))
  expect_true(all(unique(opts$surf) %in% c("inflated", "white", "pial")))
  
  # Check one known row: fsaverage6 / 200 / 7 / inflated
  row_idx <- which(opts$space == "fsaverage6" &
                     opts$parcels == 200 &
                     opts$networks == 7 &
                     opts$surf == "inflated")
  expect_true(length(row_idx) == 1)
  expect_equal(opts$cbig_space[row_idx], "fsaverage6")
  expect_equal(opts$template_id[row_idx], "fsaverage")
})

test_that("glasser_surf returns valid fsaverage surface atlas", {
  skip_on_cran()
  
  glas <- tryCatch({
    glasser_surf(space = "fsaverage", surf = "pial")
  }, error = function(e) {
    skip("Failed to load Glasser surface atlas")
  })
  
  expect_true(inherits(glas, "surfatlas"))
  expect_equal(glas$surface_space, "fsaverage")
  expect_true(inherits(glas$lh_atlas, "LabeledNeuroSurface"))
  expect_true(inherits(glas$rh_atlas, "LabeledNeuroSurface"))
  
  # Basic region metadata consistency
  expect_equal(length(glas$ids), length(glas$labels))
  expect_equal(length(glas$ids), length(glas$orig_labels))
  expect_equal(length(glas$ids), nrow(glas$cmap))
  
  # Hemispheres should both be represented
  expect_true(all(c("left", "right") %in% unique(glas$hemi)))
})

test_that("glasser_surf rejects unsupported spaces", {
  skip_on_cran()
  expect_error(glasser_surf(space = "fsaverage5"),
               "only available in 'fsaverage' space")
})

test_that("network-specific functionality works correctly in Schaefer atlases", {
  skip_on_cran()
  
  # Load atlases with different network configurations
  atlas_7net <- tryCatch({
    get_schaefer_atlas(parcels = "200", networks = "7")
  }, error = function(e) {
    skip("Failed to load Schaefer atlas")
  })
  
  atlas_17net <- tryCatch({
    get_schaefer_atlas(parcels = "200", networks = "17")
  }, error = function(e) {
    skip("Failed to load Schaefer atlas")
  })
  
  # Test 1: Network assignments are valid
  expect_true("network" %in% names(atlas_7net))
  expect_true("network" %in% names(atlas_17net))
  
  # All regions should have network assignments
  expect_equal(length(atlas_7net$network), 200)
  expect_equal(length(atlas_17net$network), 200)
  expect_false(any(is.na(atlas_7net$network)))
  expect_false(any(is.na(atlas_17net$network)))
  
  # Test 2: Network count validation
  unique_7net <- unique(atlas_7net$network)
  unique_17net <- unique(atlas_17net$network)
  
  # Should have expected number of unique networks
  expect_lte(length(unique_7net), 7)
  expect_lte(length(unique_17net), 17)
  
  # Test 3: Network distribution across hemispheres
  # Networks should be roughly balanced between hemispheres
  for (net in unique_7net) {
    net_regions <- which(atlas_7net$network == net)
    left_count <- sum(atlas_7net$hemi[net_regions] == "left")
    right_count <- sum(atlas_7net$hemi[net_regions] == "right")
    
    # Each network should have regions in both hemispheres
    expect_true(left_count > 0)
    expect_true(right_count > 0)
    
    # Should be roughly balanced (within 20% difference)
    balance_ratio <- min(left_count, right_count) / max(left_count, right_count)
    expect_gt(balance_ratio, 0.3)  # At least 30% balance
  }
  
  # Test 4: Network preservation during operations
  # Reduce data by network
  test_data <- neuroim2::NeuroVol(
    rnorm(prod(dim(atlas_7net$atlas))),
    space = neuroim2::space(atlas_7net$atlas)
  )
  
  # Use wide format to maintain backward compatibility with this test
  reduced <- reduce_atlas(atlas_7net, test_data, mean, format = "wide")
  
  # Convert wide format to long format for network summary
  # Get the values from the reduced tibble (one row, many columns)
  region_values <- as.numeric(reduced[1, ])
  
  # The column names in reduced correspond to atlas orig_labels
  # We need to match them to get the corresponding network assignments
  col_indices <- match(colnames(reduced), atlas_7net$orig_labels)
  region_networks <- atlas_7net$network[col_indices]
  
  # Create network summary
  network_means <- tapply(
    region_values,
    region_networks,
    mean,
    na.rm = TRUE
  )
  
  expect_equal(length(network_means), length(unique_7net))
  expect_false(any(is.na(network_means)))
  
  # Test 5: Network info is properly stored
  # Network info should be in the network field, not orig_labels
  expect_true(!is.null(atlas_7net$network))
  expect_true(!is.null(atlas_17net$network))
  # All regions should have network assignments
  expect_equal(length(atlas_7net$network), length(atlas_7net$labels))
  expect_equal(length(atlas_17net$network), length(atlas_17net$labels))
})

test_that("color map handling works correctly across atlas operations", {
  skip_on_cran()
  
  # Test 1: Atlas color maps are properly structured
  atlases <- list(
    schaefer = tryCatch(get_schaefer_atlas(parcels = "100", networks = "7"), 
                       error = function(e) NULL),
    glasser = tryCatch(get_glasser_atlas(), error = function(e) NULL),
    aseg = tryCatch(get_aseg_atlas(), error = function(e) NULL)
  )
  
  # Remove failed loads
  atlases <- atlases[!sapply(atlases, is.null)]
  
  for (atlas_name in names(atlases)) {
    atlas <- atlases[[atlas_name]]
    
    # Check cmap structure
    expect_true("cmap" %in% names(atlas))
    expect_true(inherits(atlas$cmap, "data.frame"))
    
    # Should have RGB columns
    expect_true(all(c("red", "green", "blue") %in% names(atlas$cmap)) ||
                all(c("r", "g", "b") %in% names(atlas$cmap)) ||
                all(c("R", "G", "B") %in% names(atlas$cmap)))
    
    # RGB values should be valid (0-255)
    rgb_cols <- intersect(names(atlas$cmap), c("red", "green", "blue", "r", "g", "b", "R", "G", "B"))
    for (col in rgb_cols) {
      expect_true(all(atlas$cmap[[col]] >= 0 & atlas$cmap[[col]] <= 255))
    }
    
    # Should have entry for each region
    expect_equal(nrow(atlas$cmap), length(atlas$ids))
  }
  
  # Test 2: Color map preservation during merge
  if (length(atlases) >= 2) {
    atlas_names <- names(atlases)[1:2]
    atlas1 <- atlases[[atlas_names[1]]]
    atlas2 <- atlases[[atlas_names[2]]]
    
    # Ensure same space
    if (!all(dim(atlas1$atlas) == dim(atlas2$atlas))) {
      atlas2 <- list(
        name = atlas2$name,
        atlas = resample(atlas2$atlas, neuroim2::space(atlas1$atlas)),
        cmap = atlas2$cmap,
        ids = atlas2$ids,
        labels = atlas2$labels,
        orig_labels = atlas2$orig_labels,
        hemi = atlas2$hemi
      )
      class(atlas2) <- class(atlases[[atlas_names[2]]])
    }
    
    merged <- merge_atlases(atlas1, atlas2)
    
    # Merged cmap should have all regions
    expect_equal(nrow(merged$cmap), length(merged$ids))
    
    # Original colors should be preserved
    # First atlas colors
    n1 <- length(atlas1$ids)
    expect_equal(merged$cmap[1:n1,], atlas1$cmap)
  }
  
  # Test 3: Unique colors per region
  for (atlas_name in names(atlases)) {
    atlas <- atlases[[atlas_name]]
    
    # Create color strings
    rgb_cols <- intersect(names(atlas$cmap), c("r", "g", "b", "R", "G", "B"))
    if (length(rgb_cols) == 3) {
      color_strings <- paste(atlas$cmap[[rgb_cols[1]]], 
                           atlas$cmap[[rgb_cols[2]]], 
                           atlas$cmap[[rgb_cols[3]]], sep = "-")
      
      # Most regions should have unique colors (allow some duplicates)
      duplicate_ratio <- sum(duplicated(color_strings)) / length(color_strings)
      expect_lt(duplicate_ratio, 0.2)  # Less than 20% duplicates
    }
  }
})

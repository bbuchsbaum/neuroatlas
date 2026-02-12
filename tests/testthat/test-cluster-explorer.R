make_toy_cluster_explorer_inputs <- function(n_time = 4) {
  dims <- c(5, 5, 5)
  sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 1L
  atlas_arr[4:5, 4:5, 4:5] <- 2L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas <- list(
    name = "toy_atlas",
    atlas = atlas_vol,
    ids = c(1L, 2L),
    labels = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi = c(NA, NA),
    roi_metadata = tibble::tibble(
      id = c(1L, 2L),
      label = c("A", "B"),
      hemi = c(NA_character_, NA_character_)
    )
  )
  class(atlas) <- c("toy", "atlas")

  stat_arr <- array(0, dim = dims)
  stat_arr[1:2, 1:2, 1:2] <- 4.5
  stat_arr[4:5, 4:5, 4:5] <- -5.5
  stat_arr[3, 3, 3] <- 9.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp3)

  sp4 <- neuroim2::NeuroSpace(dim = c(dims, n_time), spacing = c(1, 1, 1))
  data_arr <- array(0, dim = c(dims, n_time))
  for (t in seq_len(n_time)) {
    data_arr[1:2, 1:2, 1:2, t] <- t
    data_arr[4:5, 4:5, 4:5, t] <- 10 + t
  }
  data_vec <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    condition = rep(c("A", "B"), length.out = n_time),
    trial = seq_len(n_time)
  )

  list(
    atlas = atlas,
    stat_map = stat_map,
    data_vec = data_vec,
    sample_table = sample_table
  )
}

test_that("build_cluster_explorer_data separates positive and negative clusters",
          {
  x <- make_toy_cluster_explorer_inputs(n_time = 4)

  res <- suppressWarnings(
    build_cluster_explorer_data(
      data_source = x$data_vec,
      atlas = x$atlas,
      stat_map = x$stat_map,
      sample_table = x$sample_table,
      threshold = 3,
      min_cluster_size = 4,
      connectivity = "26-connect",
      tail = "two_sided"
    )
  )

  expect_true(is.list(res))
  expect_true(all(c("cluster_table", "cluster_parcels", "cluster_ts") %in%
                    names(res)))

  ct <- res$cluster_table
  expect_equal(nrow(ct), 2)
  expect_true(all(c("positive", "negative") %in% ct$sign))
  expect_true(all(ct$n_voxels >= 4))
  expect_true(all(grepl("^[PN]", ct$cluster_id)))
  expect_true(all(ct$atlas_label_primary %in% c("A", "B")))
  expect_true(all(ct$n_parcels >= 1))

  cp <- res$cluster_parcels
  expect_true(nrow(cp) >= 2)
  expect_true(all(cp$parcel_label %in% c("A", "B")))

  ts <- res$cluster_ts
  expect_equal(length(unique(ts$cluster_id)), 2)
  expect_equal(length(unique(ts$.sample_index)), 4)
  expect_true(all(c("condition", "trial") %in% names(ts)))
})

test_that("build_cluster_explorer_data enforces sample length match", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)
  bad_sample <- tibble::tibble(condition = c("A", "B"))

  expect_error(
    build_cluster_explorer_data(
      data_source = x$data_vec,
      atlas = x$atlas,
      stat_map = x$stat_map,
      sample_table = bad_sample
    ),
    "must equal number of samples"
  )
})

test_that("infer_design_var_type classifies variables as expected", {
  expect_equal(infer_design_var_type(1:5), "continuous")
  expect_equal(infer_design_var_type(Sys.Date() + 1:3), "continuous")
  expect_equal(infer_design_var_type(factor(c("a", "b"))), "categorical")
  expect_equal(infer_design_var_type(c(TRUE, FALSE)), "categorical")
})

test_that("parcel values helper supports sign display modes", {
  cp <- tibble::tibble(
    cluster_id = c("P1", "N2"),
    sign = c("positive", "negative"),
    parcel_id = c(1L, 1L),
    parcel_label = c("A", "A"),
    n_voxels = c(8L, 4L),
    frac = c(0.8, 0.2),
    peak_stat = c(4.2, -5.6),
    max_pos = c(4.2, NA_real_),
    min_neg = c(NA_real_, -5.6)
  )

  ids <- c(1L, 2L)
  dominant <- neuroatlas:::.parcel_values_from_clusters(cp, ids, mode = "dominant")
  positive <- neuroatlas:::.parcel_values_from_clusters(cp, ids,
                                                        mode = "positive_only")
  negative <- neuroatlas:::.parcel_values_from_clusters(cp, ids,
                                                        mode = "negative_only")

  expect_equal(dominant[["1"]], -5.6)
  expect_equal(positive[["1"]], 4.2)
  expect_equal(negative[["1"]], -5.6)
  expect_true(is.na(dominant[["2"]]))
})

test_that("cluster_explorer constructs a shiny app object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("ggiraph")

  x <- make_toy_cluster_explorer_inputs(n_time = 3)
  fake_surfatlas <- list(
    ids = c(1L, 2L),
    labels = c("A", "B"),
    hemi = c("left", "right")
  )
  class(fake_surfatlas) <- c("toy_surf", "surfatlas", "atlas")

  app <- cluster_explorer(
    data_source = x$data_vec,
    atlas = x$atlas,
    stat_map = x$stat_map,
    surfatlas = fake_surfatlas,
    sample_table = x$sample_table
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("cluster_explorer supports zero-argument demo mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not_installed("ggiraph")

  app <- suppressMessages(cluster_explorer())
  expect_s3_class(app, "shiny.appobj")
})

test_that("build_design_plot evaluates aesthetics without pronoun errors", {
  dat <- tibble::tibble(
    cluster_id = rep(c("P1", "N2"), each = 4),
    signal = c(1, 2, 3, 4, 4, 3, 2, 1),
    .sample_index = rep(1:4, times = 2)
  )

  p <- neuroatlas:::.build_design_plot(dat, ".sample_index")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("analysis plugin hooks transform extracted signal data", {
  dat <- tibble::tibble(
    cluster_id = rep("P1", 4),
    signal = 1:4,
    .sample_index = 1:4
  )
  design <- tibble::tibble(.sample_index = 1:4, x = 11:14)
  plugin <- list(
    id = "scale",
    label = "Scale",
    run = function(ts_data, design, params, context) {
      list(
        data = dplyr::mutate(ts_data, signal = signal * params$mult),
        design = design,
        diagnostics = list(mult = params$mult)
      )
    }
  )

  norm <- neuroatlas:::.normalize_analysis_plugins(
    analysis_plugins = list(scale = plugin),
    default_plugin = "none"
  )
  out <- neuroatlas:::.run_analysis_plugin(
    plugin = norm[["scale"]],
    ts_data = dat,
    design = design,
    params = list(mult = 3),
    context = list()
  )

  expect_equal(out$data$signal, c(3, 6, 9, 12))
  expect_equal(out$diagnostics$mult, 3)
})

test_that("custom selection engine delegates to provider", {
  x <- make_toy_cluster_explorer_inputs(n_time = 3)

  provider <- function(...) {
    res <- suppressWarnings(build_cluster_explorer_data(
      data_source = x$data_vec,
      atlas = x$atlas,
      stat_map = x$stat_map,
      sample_table = x$sample_table,
      min_cluster_size = 4
    ))
    res$prefetch_info$provider <- "custom"
    res
  }

  out <- neuroatlas:::.compute_selection_data(
    selection_engine = "custom",
    selection_provider = provider
  )
  expect_true(is.list(out))
  expect_equal(out$prefetch_info$provider, "custom")
})

test_that("parcel selection engine builds parcel-backed selections", {
  x <- make_toy_cluster_explorer_inputs(n_time = 4)
  out <- suppressWarnings(neuroatlas:::.compute_selection_data(
    selection_engine = "parcel",
    data_source = x$data_vec,
    atlas = x$atlas,
    stat_map = x$stat_map,
    sample_table = x$sample_table,
    threshold = 1,
    min_cluster_size = 4,
    tail = "two_sided",
    parcel_ids = 1L
  ))

  expect_true(is.list(out))
  expect_equal(nrow(out$cluster_table), 1)
  expect_true(grepl("^R", out$cluster_table$cluster_id[1]))
  expect_equal(length(unique(out$cluster_ts$.sample_index)), 4)
})

test_that("sphere selection engine builds spherical selections", {
  x <- make_toy_cluster_explorer_inputs(n_time = 4)
  out <- suppressWarnings(neuroatlas:::.compute_selection_data(
    selection_engine = "sphere",
    data_source = x$data_vec,
    atlas = x$atlas,
    stat_map = x$stat_map,
    sample_table = x$sample_table,
    threshold = 1,
    min_cluster_size = 2,
    tail = "two_sided",
    sphere_centers = c(2, 2, 2),
    sphere_radius = 1.5,
    sphere_units = "voxels",
    sphere_combine = "separate"
  ))

  expect_true(is.list(out))
  expect_true(nrow(out$cluster_table) >= 1)
  expect_true(all(grepl("^S", out$cluster_table$cluster_id)))
  expect_equal(length(unique(out$cluster_ts$.sample_index)), 4)
})

test_that("build_cluster_explorer_data supports custom series_fun duck typing", {
  x <- make_toy_cluster_explorer_inputs(n_time = 4)
  stat_arr <- as.array(x$stat_map)
  stat_arr[3, 3, 3] <- 0
  stat_map <- neuroim2::NeuroVol(stat_arr, space = neuroim2::space(x$stat_map))

  data_source <- list(n_samples = 4L)

  call_count <- 0L
  series_fun <- function(src, voxel_coords) {
    call_count <<- call_count + 1L
    out <- matrix(0, nrow = src$n_samples, ncol = nrow(voxel_coords))
    is_a <- voxel_coords[, 1] <= 2 & voxel_coords[, 2] <= 2 & voxel_coords[, 3] <= 2
    is_b <- voxel_coords[, 1] >= 4 & voxel_coords[, 2] >= 4 & voxel_coords[, 3] >= 4
    for (t in seq_len(src$n_samples)) {
      out[t, is_a] <- t
      out[t, is_b] <- 10 + t
    }
    out
  }

  res <- suppressWarnings(
    build_cluster_explorer_data(
      data_source = data_source,
      atlas = x$atlas,
      stat_map = stat_map,
      sample_table = x$sample_table,
      threshold = 3,
      min_cluster_size = 4,
      tail = "two_sided",
      series_fun = series_fun
    )
  )

  expect_true(call_count > 0)
  expect_equal(length(unique(res$cluster_ts$.sample_index)), data_source$n_samples)
  expect_true(all(is.finite(res$cluster_ts$signal)))
})

test_that("prefetch guards can skip eager series extraction", {
  x <- make_toy_cluster_explorer_inputs(n_time = 4)
  calls <- 0L
  series_fun <- function(src, voxel_coords) {
    calls <<- calls + 1L
    matrix(0, nrow = src$n_samples, ncol = nrow(voxel_coords))
  }

  data_source <- list(n_samples = 4L)
  res <- suppressWarnings(
    build_cluster_explorer_data(
      data_source = data_source,
      atlas = x$atlas,
      stat_map = x$stat_map,
      sample_table = x$sample_table,
      min_cluster_size = 4,
      series_fun = series_fun,
      prefetch = TRUE,
      prefetch_max_clusters = 1,
      prefetch_max_voxels = 10
    )
  )

  expect_equal(calls, 0)
  expect_equal(nrow(res$cluster_ts), 0)
  expect_true(isTRUE(res$prefetch_info$requested))
  expect_false(isTRUE(res$prefetch_info$applied))
})

test_that("plot_brain selection id parser handles encoded and legacy ids", {
  ids <- c("Left Lateral::7::21", "9", "bad-id")
  parsed <- neuroatlas:::.parse_plot_brain_selection_ids(ids)

  expect_equal(nrow(parsed), 3)
  expect_equal(parsed$parcel_id[1], 7L)
  expect_equal(parsed$shape_id[1], 21L)
  expect_equal(parsed$panel[1], "Left Lateral")
  expect_equal(parsed$parcel_id[2], 9L)
  expect_true(is.na(parsed$shape_id[2]))
  expect_true(is.na(parsed$parcel_id[3]))
})

test_that("surface pick lookup maps polygon ids to nearest surface vertex", {
  sp <- neuroim2::NeuroSpace(
    dim = c(6, 6, 6),
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0)
  )
  stat_map <- neuroim2::NeuroVol(array(0, dim = c(6, 6, 6)), space = sp)

  panel_ctx <- list(
    "Left Lateral" = list(
      xy = matrix(c(
        2, 2,
        4, 2,
        2, 4,
        4, 4
      ), ncol = 2, byrow = TRUE),
      verts = matrix(c(
        2, 2, 2,
        4, 2, 2,
        2, 4, 2,
        4, 4, 2
      ), ncol = 3, byrow = TRUE),
      parcels = c(1L, 1L, 1L, 1L),
      geometry = NULL
    )
  )

  poly <- tibble::tibble(
    panel = rep("Left Lateral", 3),
    parcel_id = rep(1L, 3),
    poly_id = rep(10L, 3),
    x = c(3.6, 3.8, 3.7),
    y = c(3.6, 3.5, 3.8)
  )

  lookup <- neuroatlas:::.surface_pick_lookup_from_polygons(
    poly = poly,
    panel_ctx = panel_ctx,
    stat_map = stat_map
  )

  expect_equal(nrow(lookup), 1)
  expect_equal(lookup$vertex_index[[1]], 4L)
  expect_equal(c(lookup$surface_x[[1]], lookup$surface_y[[1]], lookup$surface_z[[1]]),
               c(4, 4, 2))

  parsed <- neuroatlas:::.parse_plot_brain_selection_ids(lookup$data_id[[1]])
  expect_equal(parsed$panel[[1]], "Left Lateral")
  expect_equal(parsed$parcel_id[[1]], 1L)
  expect_equal(parsed$shape_id[[1]], 10L)
})

test_that("surface pick cluster matcher respects radius and nearest fallback", {
  cluster_voxels <- list(
    A = matrix(c(2, 2, 2,
                 2, 2, 3), ncol = 3, byrow = TRUE),
    B = matrix(c(5, 5, 5,
                 5, 5, 4), ncol = 3, byrow = TRUE)
  )

  ids_near <- neuroatlas:::.clusters_for_grid_centers(
    cluster_voxels = cluster_voxels,
    centers = matrix(c(2, 2, 2), ncol = 3),
    radius = 0,
    fallback_nearest = TRUE
  )
  expect_equal(ids_near[1], "A")

  ids_radius <- neuroatlas:::.clusters_for_grid_centers(
    cluster_voxels = cluster_voxels,
    centers = matrix(c(4.2, 4.2, 4.2), ncol = 3),
    radius = 2,
    fallback_nearest = FALSE
  )
  expect_equal(ids_radius[1], "B")

  ids_none <- neuroatlas:::.clusters_for_grid_centers(
    cluster_voxels = cluster_voxels,
    centers = matrix(c(4.2, 4.2, 4.2), ncol = 3),
    radius = 0,
    fallback_nearest = FALSE
  )
  expect_equal(length(ids_none), 0)
})

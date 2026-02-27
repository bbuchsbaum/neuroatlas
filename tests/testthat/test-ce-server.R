skip_if_not_installed("shiny")
skip_if_not_installed("DT")
skip_if_not_installed("ggiraph")

# ---------------------------------------------------------------------------
# Shared setup: build toy inputs and wrap .cluster_explorer_server() in a
# standard server closure. A shared env bridges the server's internal
# reactives to the test expression.
# ---------------------------------------------------------------------------
.make_test_setup <- function() {
  toy <- make_toy_cluster_explorer_inputs(n_time = 4)
  surfatlas <- make_toy_surfatlas()

  plugins <- neuroatlas:::.normalize_analysis_plugins(
    analysis_plugins = NULL,
    default_plugin = "none"
  )

  sample_tbl <- neuroatlas:::.normalize_sample_table(
    sample_table = toy$sample_table,
    n_samples = 4L
  )

  bridge <- new.env(parent = emptyenv())

  server_fn <- function(input, output, session) {
    rv <- neuroatlas:::.cluster_explorer_server(
      input = input,
      output = output,
      session = session,
      surfatlas = surfatlas,
      stat_map = toy$stat_map,
      data_source = toy$data_vec,
      atlas = toy$atlas,
      sample_tbl = sample_tbl,
      series_fun = NULL,
      selection_engine = "cluster",
      selection_provider = NULL,
      parcel_ids = NULL,
      sphere_centers = NULL,
      sphere_radius = 6,
      sphere_units = "mm",
      sphere_combine = "separate",
      plugins = plugins,
      default_analysis_plugin = "none",
      overlay_space = NULL,
      overlay_density = NULL,
      overlay_resolution = NULL,
      palette = "vik",
      threshold = 3,
      min_cluster_size = 1L
    )
    bridge$rv <- rv
  }

  list(server_fn = server_fn, bridge = bridge)
}

# Helper: set the required initial inputs so the server can boot, then
# trigger Apply so computed() fires.
.boot_server <- function(session) {
  session$setInputs(
    threshold = 3,
    min_cluster_size = 1L,
    connectivity = "26-connect",
    tail = "two_sided",
    prefetch_mode = TRUE,
    prefetch_max_clusters = 200L,
    prefetch_max_voxels = 100000L,
    map_scope = "all_clusters",
    display_mode = "dominant",
    brain_click_mode = "parcel",
    surface_pick_radius = 2,
    show_cluster_overlay = FALSE,
    overlay_threshold = 1e-06,
    overlay_alpha = 0.45,
    overlay_fun = "avg",
    overlay_space_ui = "auto",
    overlay_sampling = "midpoint",
    x_var = ".sample_index",
    collapse_vars = character(0),
    analysis_plugin_id = "none",
    apply_btn = 0L
  )
}

# ---------------------------------------------------------------------------
# 1. computed() reactive fires on apply and returns expected structure
# ---------------------------------------------------------------------------
test_that("computed() fires and returns valid structure", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    dat <- rv$computed()

    expect_type(dat, "list")
    expect_true("cluster_table" %in% names(dat))
    expect_true("cluster_ts" %in% names(dat))
    expect_true("cluster_voxels" %in% names(dat))
    expect_true("cluster_parcels" %in% names(dat))
    expect_true("sample_table" %in% names(dat))
    expect_s3_class(dat$cluster_table, "data.frame")
    expect_s3_class(dat$cluster_ts, "data.frame")
  })
})

# ---------------------------------------------------------------------------
# 2. Cluster table has expected columns
# ---------------------------------------------------------------------------
test_that("cluster table contains expected columns", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    tbl <- rv$computed()$cluster_table

    expected_cols <- c("cluster_id", "sign", "n_voxels", "max_stat",
                       "atlas_label_primary", "n_parcels",
                       "parcel_overlap", "peak_coord")
    if (nrow(tbl) > 0) {
      for (col in expected_cols) {
        expect_true(col %in% names(tbl),
                    info = paste("Missing column:", col))
      }
    }
  })
})

# ---------------------------------------------------------------------------
# 3. Selection state updates when table rows are selected
# ---------------------------------------------------------------------------
test_that("table row selection updates sel_state", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    tbl <- rv$computed()$cluster_table

    if (nrow(tbl) >= 1) {
      session$setInputs(cluster_table_rows_selected = 1L)
      expect_length(rv$sel_state$cluster_ids, 1)
      expect_equal(rv$sel_state$cluster_ids,
                   as.character(tbl$cluster_id[1]))
      expect_equal(rv$sel_state$source, "table")
    }
  })
})

# ---------------------------------------------------------------------------
# 4. selected_cluster_ids() falls back to first cluster when empty
# ---------------------------------------------------------------------------
test_that("selected_cluster_ids() defaults to first cluster", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    tbl <- rv$computed()$cluster_table

    if (nrow(tbl) > 0) {
      ids <- rv$selected_cluster_ids()
      expect_true(length(ids) >= 1)
      expect_true(ids[1] %in% tbl$cluster_id)
    }
  })
})

# ---------------------------------------------------------------------------
# 5. Prefetch status text reflects state
# ---------------------------------------------------------------------------
test_that("prefetch status text reflects state", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    status_text <- output$prefetch_status
    expect_type(status_text, "character")
  })
})

# ---------------------------------------------------------------------------
# 6. Analysis plugin state updates on apply
# ---------------------------------------------------------------------------
test_that("analysis plugin apply updates analysis_state", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    expect_equal(rv$analysis_state$applied_plugin_id, "none")

    session$setInputs(analysis_plugin_id = "none")
    session$setInputs(analysis_apply_btn = 1L)

    expect_equal(rv$analysis_state$applied_plugin_id, "none")
    expect_type(rv$analysis_state$applied_params, "list")
  })
})

# ---------------------------------------------------------------------------
# 7. Cluster table CSV round-trip
# ---------------------------------------------------------------------------
test_that("cluster table is available for CSV download", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)

    rv <- setup$bridge$rv
    tbl <- rv$computed()$cluster_table

    expect_s3_class(tbl, "data.frame")
    if (nrow(tbl) > 0) {
      expect_true("cluster_id" %in% names(tbl))

      tmp <- tempfile(fileext = ".csv")
      on.exit(unlink(tmp), add = TRUE)
      utils::write.csv(tbl, file = tmp, row.names = FALSE)
      roundtrip <- utils::read.csv(tmp)
      expect_equal(nrow(roundtrip), nrow(tbl))
      expect_true("cluster_id" %in% names(roundtrip))
    }
  })
})

# ---------------------------------------------------------------------------
# 8. Reset button fires without error
# ---------------------------------------------------------------------------
test_that("reset button fires without error", {
  setup <- .make_test_setup()

  shiny::testServer(setup$server_fn, {
    .boot_server(session)
    session$setInputs(apply_btn = 1L)
    session$setInputs(threshold = 10, min_cluster_size = 50L)
    session$setInputs(reset_filters_btn = 1L)
    expect_true(TRUE)
  })
})

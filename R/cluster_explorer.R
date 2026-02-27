.ce_label_with_help <- function(label, help_text) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    return(label)
  }
  htmltools::tagList(
    label,
    shiny::tags$span(
      class = "ce-help-icon",
      title = help_text,
      `aria-label` = help_text,
      "?"
    )
  )
}

.cluster_explorer_css <- function() {
  css_path <- system.file("www", "cluster-explorer.css", package = "neuroatlas")
  if (!nzchar(css_path)) return("")
  paste(readLines(css_path, warn = FALSE), collapse = "\n")
}

.cluster_explorer_server <- function(input, output, session,
                                     surfatlas,
                                     stat_map,
                                     data_source,
                                     atlas,
                                     sample_tbl,
                                     series_fun,
                                     selection_engine,
                                     selection_provider,
                                     parcel_ids,
                                     sphere_centers,
                                     sphere_radius,
                                     sphere_units,
                                     sphere_combine,
                                     plugins,
                                     default_analysis_plugin,
                                     overlay_space,
                                     overlay_density,
                                     overlay_resolution,
                                     palette,
                                     threshold,
                                     min_cluster_size) {
  use_surface_plot <- tryCatch(
    .has_surface_geometry(surfatlas),
    error = function(e) FALSE
  )
  brain_views <- c("lateral", "medial")
  brain_hemis <- c("left", "right")

  sel_state <- shiny::reactiveValues(
    cluster_ids = character(0),
    source = "init",
    stamp = Sys.time()
  )
  cache_state <- shiny::reactiveValues(
    series_cache = new.env(parent = emptyenv()),
    ts_cache = new.env(parent = emptyenv())
  )
  surface_pick_lookup_cache <- local({
    tbl <- NULL
    function() {
      if (is.null(tbl)) {
        tbl <<- .build_plot_brain_surface_pick_lookup(
          surfatlas = surfatlas,
          stat_map = stat_map,
          views = brain_views,
          hemis = brain_hemis,
          surface = "inflated"
        )
      }
      tbl
    }
  })
  analysis_state <- shiny::reactiveValues(
    applied_plugin_id = default_analysis_plugin,
    applied_params = list(),
    stamp = Sys.time()
  )
  analysis_drawer_open <- shiny::reactiveVal(FALSE)

  set_analysis_drawer <- function(open = FALSE) {
    is_open <- isTRUE(open)
    analysis_drawer_open(is_open)
    session$sendCustomMessage("ceAnalysisDrawer", list(open = is_open))
  }

  shiny::observeEvent(input$reset_filters_btn, {
    shiny::updateNumericInput(session, "threshold", value = threshold)
    shiny::updateNumericInput(session, "min_cluster_size",
                              value = min_cluster_size)
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$analysis_open_btn, {
    set_analysis_drawer(TRUE)
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$analysis_close_btn, {
    set_analysis_drawer(FALSE)
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$analysis_drawer_dismiss, {
    set_analysis_drawer(FALSE)
  }, ignoreNULL = TRUE)

  computed <- shiny::eventReactive(input$apply_btn, {
    thresh_val <- suppressWarnings(as.numeric(input$threshold))
    mcs_val <- suppressWarnings(as.integer(input$min_cluster_size))
    shiny::validate(
      shiny::need(is.finite(thresh_val) && thresh_val > 0,
                   "Threshold must be a positive number."),
      shiny::need(is.finite(mcs_val) && mcs_val >= 1,
                   "Min cluster size must be an integer >= 1.")
    )

    max_clusters <- suppressWarnings(as.numeric(input$prefetch_max_clusters))
    max_voxels <- suppressWarnings(as.numeric(input$prefetch_max_voxels))
    if (!is.finite(max_clusters) || max_clusters < 1) max_clusters <- Inf
    if (!is.finite(max_voxels) || max_voxels < 1) max_voxels <- Inf

    .compute_selection_data(
      selection_engine = selection_engine,
      selection_provider = selection_provider,
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_tbl,
      threshold = thresh_val,
      min_cluster_size = mcs_val,
      connectivity = input$connectivity,
      tail = input$tail,
      series_fun = series_fun,
      prefetch = isTRUE(input$prefetch_mode),
      prefetch_max_clusters = max_clusters,
      prefetch_max_voxels = max_voxels,
      series_cache_env = cache_state$series_cache,
      parcel_ids = parcel_ids,
      sphere_centers = sphere_centers,
      sphere_radius = sphere_radius,
      sphere_units = sphere_units,
      sphere_combine = sphere_combine
    )
  }, ignoreNULL = FALSE)

  set_selection <- function(ids, source) {
    ids <- unique(as.character(ids))
    ids <- ids[nzchar(ids)]
    sel_state$cluster_ids <- ids
    sel_state$source <- source
    sel_state$stamp <- Sys.time()
  }

  shiny::observeEvent(computed(), {
    dat <- computed()
    cache_state$series_cache <- new.env(parent = emptyenv())
    cache_state$ts_cache <- new.env(parent = emptyenv())

    if (nrow(dat$cluster_ts) > 0) {
      split_ts <- split(dat$cluster_ts, dat$cluster_ts$cluster_id)
      for (cid in names(split_ts)) {
        assign(cid, split_ts[[cid]], envir = cache_state$ts_cache)
      }
    }

    ids <- dat$cluster_table$cluster_id
    if (length(ids) > 0) {
      set_selection(ids[1], source = "recompute")
    } else {
      set_selection(character(0), source = "recompute")
    }

    design_cols <- setdiff(names(dat$sample_table), ".sample_index")
    x_choices <- c(".sample_index", design_cols)
    x_selected <- if ("time" %in% design_cols) "time" else ".sample_index"

    shiny::updateSelectInput(
      session = session,
      inputId = "x_var",
      choices = x_choices,
      selected = x_selected
    )
    shiny::updateSelectizeInput(
      session = session,
      inputId = "collapse_vars",
      choices = design_cols,
      selected = character(0),
      server = TRUE
    )

    analysis_state$applied_plugin_id <- "none"
    analysis_state$applied_params <- list()
    analysis_state$stamp <- Sys.time()
  }, ignoreNULL = FALSE)

  current_plugin <- shiny::reactive({
    pid <- input$analysis_plugin_id
    if (is.null(pid) || !nzchar(pid)) pid <- "none"
    plug <- plugins[[pid]]
    if (is.null(plug)) {
      plugins[["none"]]
    } else {
      plug
    }
  })

  output$analysis_params_ui <- shiny::renderUI({
    plugin <- current_plugin()
    .analysis_plugin_param_ui(plugin)
  })

  shiny::observeEvent(input$analysis_apply_btn, {
    plugin <- current_plugin()
    analysis_state$applied_plugin_id <- plugin$id
    analysis_state$applied_params <- .collect_analysis_params(
      input = input,
      plugin = plugin
    )
    analysis_state$stamp <- Sys.time()
    set_analysis_drawer(FALSE)
  }, ignoreNULL = TRUE)

  output$prefetch_status <- shiny::renderText({
    info <- computed()$prefetch_info
    if (is.null(info)) return("")
    if (isTRUE(info$applied)) {
      paste0(
        "Prefetch: enabled (", info$n_clusters, " clusters, ",
        info$total_voxels, " voxels)"
      )
    } else if (isTRUE(info$requested)) {
      paste0(
        "Prefetch skipped by limits (clusters=", info$n_clusters, "/",
        info$max_clusters, "; voxels=", info$total_voxels, "/",
        info$max_voxels, "). Lazy cache mode active."
      )
    } else {
      "Prefetch disabled. Lazy cache mode active."
    }
  })

  output$cluster_table <- DT::renderDT({
    tbl <- computed()$cluster_table
    if (nrow(tbl) == 0) {
      return(DT::datatable(
        tbl,
        class = "compact stripe hover",
        options = list(pageLength = 8, scrollX = TRUE)
      ))
    }

    show_tbl <- tbl[, c("cluster_id", "sign", "n_voxels",
                        "max_stat", "atlas_label_primary",
                        "n_parcels", "parcel_overlap", "peak_coord"),
                    drop = FALSE]

    DT::datatable(
      show_tbl,
      rownames = FALSE,
      selection = "multiple",
      class = "compact stripe hover order-column row-border",
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "ftip",
        order = list(list(3, "desc"))
      )
    )
  })

  shiny::observeEvent(input$cluster_table_rows_selected, {
    rows <- input$cluster_table_rows_selected
    tbl <- computed()$cluster_table
    if (length(rows) == 0 || nrow(tbl) == 0) {
      return(invisible(NULL))
    }
    rows <- rows[rows >= 1 & rows <= nrow(tbl)]
    if (length(rows) > 0) {
      set_selection(tbl$cluster_id[rows], source = "table")
    }
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$replot_table_btn, {
    rows <- input$cluster_table_rows_selected
    tbl <- computed()$cluster_table
    if (length(rows) == 0 || nrow(tbl) == 0) {
      return(invisible(NULL))
    }
    rows <- rows[rows >= 1 & rows <= nrow(tbl)]
    if (length(rows) > 0) {
      set_selection(tbl$cluster_id[rows], source = "table_button")
    }
  }, ignoreNULL = TRUE)

  shiny::observeEvent(input$brain_plot_selected, {
    sel_ids <- input$brain_plot_selected
    parsed <- .parse_plot_brain_selection_ids(sel_ids)
    parcel_ids <- parsed$parcel_id[!is.na(parsed$parcel_id)]
    parcel_ids <- unique(as.integer(parcel_ids))
    if (length(parcel_ids) == 0) {
      return(invisible(NULL))
    }

    mode <- input$brain_click_mode
    if (is.null(mode) || !nzchar(mode)) mode <- "parcel"
    mode <- as.character(mode)
    if (!mode %in% c("parcel", "surface_pick")) {
      mode <- "parcel"
    }

    ids <- character(0)
    if (identical(mode, "surface_pick")) {
      radius <- suppressWarnings(as.numeric(input$surface_pick_radius))
      if (!is.finite(radius) || radius < 0) radius <- 0

      lookup <- surface_pick_lookup_cache()

      hit <- lookup[lookup$data_id %in% parsed$raw_id, , drop = FALSE]
      if (nrow(hit) == 0 && length(parcel_ids) > 0) {
        hit <- lookup[lookup$parcel_id %in% parcel_ids, , drop = FALSE]
      }
      if (nrow(hit) > 0) {
        centers <- as.matrix(unique(hit[, c("grid_x", "grid_y", "grid_z"),
                                       drop = FALSE]))
        ids <- .clusters_for_grid_centers(
          cluster_voxels = computed()$cluster_voxels,
          centers = centers,
          radius = radius,
          fallback_nearest = TRUE
        )
      }
    }
    if (length(ids) == 0) {
      ids <- .clusters_for_parcels(
        cluster_parcels = computed()$cluster_parcels,
        parcel_ids = parcel_ids
      )
    }
    if (length(ids) > 0) {
      set_selection(ids, source = "brain")

      proxy <- DT::dataTableProxy("cluster_table")
      tbl <- computed()$cluster_table
      rows <- match(ids, tbl$cluster_id)
      rows <- rows[!is.na(rows)]
      DT::selectRows(proxy, rows)
    }
  }, ignoreNULL = TRUE)

  selected_cluster_ids <- shiny::reactive({
    ids <- sel_state$cluster_ids
    tbl_ids <- computed()$cluster_table$cluster_id
    ids <- intersect(ids, tbl_ids)
    if (length(ids) == 0 && length(tbl_ids) > 0) {
      tbl_ids[1]
    } else {
      ids
    }
  })

  brain_scope_ids <- shiny::reactive({
    if (input$map_scope == "selected_clusters") {
      selected_cluster_ids()
    } else {
      computed()$cluster_table$cluster_id
    }
  })

  cluster_ts_selected <- shiny::reactive({
    dat <- computed()
    ids <- selected_cluster_ids()
    .resolve_cluster_ts(
      dat = dat,
      selected_cluster_ids = ids,
      data_source = data_source,
      series_fun = series_fun,
      signal_fun = mean,
      signal_fun_args = list(na.rm = TRUE),
      series_cache_env = cache_state$series_cache,
      ts_cache_env = cache_state$ts_cache
    )
  })

  analyzed_ts_selected <- shiny::reactive({
    ts_tbl <- cluster_ts_selected()
    dat <- computed()
    pid <- analysis_state$applied_plugin_id
    if (is.null(pid) || !nzchar(pid)) pid <- "none"
    plugin <- plugins[[pid]]
    if (is.null(plugin)) plugin <- plugins[["none"]]

    .run_analysis_plugin(
      plugin = plugin,
      ts_data = ts_tbl,
      design = dat$sample_table,
      params = analysis_state$applied_params,
      context = list(
        selected_cluster_ids = selected_cluster_ids(),
        cluster_table = dat$cluster_table,
        cluster_parcels = dat$cluster_parcels,
        selection_engine = selection_engine
      )
    )
  })

  cluster_overlay_payload <- shiny::reactive({
    if (!isTRUE(input$show_cluster_overlay)) return(NULL)
    if (!isTRUE(use_surface_plot)) {
      return(list(
        overlay = NULL,
        diagnostics = list(
          status = "skipped",
          reason = "Overlay projection requires surfatlas$lh_atlas/rh_atlas geometry."
        )
      ))
    }

    dat <- computed()
    scope_ids <- brain_scope_ids()
    if (length(scope_ids) == 0) return(NULL)

    ovl_thresh <- suppressWarnings(as.numeric(input$overlay_threshold))
    ovl_alpha <- suppressWarnings(as.numeric(input$overlay_alpha))
    shiny::validate(
      shiny::need(is.finite(ovl_thresh), "Overlay threshold must be a number."),
      shiny::need(is.finite(ovl_alpha) && ovl_alpha >= 0 && ovl_alpha <= 1,
                   "Overlay alpha must be between 0 and 1.")
    )

    space_override_ui <- input$overlay_space_ui
    space_override_eff <- if (is.null(space_override_ui) ||
                              identical(space_override_ui, "auto")) {
      overlay_space
    } else {
      as.character(space_override_ui)
    }

    tryCatch({
      cl_vol <- .build_cluster_overlay_volume(
        stat_map = stat_map,
        cluster_voxels = dat$cluster_voxels,
        selected_cluster_ids = scope_ids
      )

      proj <- .project_cluster_overlay(
        cluster_vol = cl_vol,
        surfatlas = surfatlas,
        space_override = space_override_eff,
        density_override = overlay_density,
        resolution_override = overlay_resolution,
        fun = input$overlay_fun,
        sampling = input$overlay_sampling
      )

      list(
        overlay = proj$overlay,
        diagnostics = .overlay_projection_diagnostics(
          cluster_vol = cl_vol,
          projection = proj,
          threshold = max(abs(ovl_thresh), .Machine$double.eps),
          sampling = input$overlay_sampling,
          fun = input$overlay_fun
        )
      )
    }, error = function(e) {
      list(
        overlay = NULL,
        diagnostics = list(
          status = "error",
          reason = conditionMessage(e)
        )
      )
    })
  })

  output$brain_plot <- ggiraph::renderGirafe({
    dat <- computed()
    surf_ids <- as.integer(surfatlas$ids)
    overlay_payload <- cluster_overlay_payload()
    overlay_vals <- if (!is.null(overlay_payload)) overlay_payload$overlay else NULL

    scope_ids <- brain_scope_ids()

    vals <- .parcel_values_from_clusters(
      cluster_parcels = dat$cluster_parcels,
      atlas_ids = surf_ids,
      selected_cluster_ids = scope_ids,
      mode = input$display_mode
    )

    lim <- NULL
    finite_vals <- vals[is.finite(vals)]
    if (length(finite_vals) > 0) {
      lim_max <- max(abs(finite_vals))
      lim <- c(-lim_max, lim_max)
    }

    g <- .fallback_brain_plot(
      surfatlas = surfatlas,
      vals = vals,
      palette = palette,
      lim = lim,
      interactive = TRUE
    )
    if (isTRUE(use_surface_plot)) {
      g <- tryCatch(
        plot_brain(
          surfatlas = surfatlas,
          vals = vals,
          views = brain_views,
          hemis = brain_hemis,
          palette = palette,
          lim = lim,
          data_id_mode = "polygon",
          overlay = overlay_vals,
          overlay_threshold = max(abs(input$overlay_threshold), .Machine$double.eps),
          overlay_alpha = input$overlay_alpha,
          overlay_palette = palette,
          interactive = TRUE
        ),
        error = function(e) {
          .fallback_brain_plot(
            surfatlas = surfatlas,
            vals = vals,
            palette = palette,
            lim = lim,
            interactive = TRUE,
            title = paste0("Parcel Layout (Fallback): ", conditionMessage(e))
          )
        }
      )
    }

    if (inherits(g, "girafe")) {
      g <- ggiraph::girafe_options(
        g,
        ggiraph::opts_hover(css = "stroke:#111;stroke-width:0.7;"),
        ggiraph::opts_hover_inv(css = "opacity:0.35;"),
        ggiraph::opts_selection(type = "multiple",
                                css = "stroke:#111;stroke-width:1.1;opacity:1;"),
        ggiraph::opts_toolbar(saveaspng = TRUE, position = "topright"),
        ggiraph::opts_tooltip(
          css = paste(
            "background:#111827;color:#f9fafb;border:none;border-radius:8px;",
            "padding:7px 9px;font-size:11px;font-family:Geist,IBM Plex Sans,sans-serif;"
          )
        )
      )
    }

    g
  })

  output$overlay_diag <- shiny::renderPrint({
    if (!isTRUE(input$show_cluster_overlay)) {
      cat("Overlay disabled.\n")
      return(invisible(NULL))
    }
    payload <- cluster_overlay_payload()
    if (is.null(payload) || is.null(payload$diagnostics)) {
      cat("No overlay diagnostics available.\n")
      return(invisible(NULL))
    }
    print(payload$diagnostics)
  })

  output$analysis_diag <- shiny::renderPrint({
    out <- analyzed_ts_selected()
    if (is.null(out$diagnostics)) {
      cat("No analysis diagnostics.\n")
      return(invisible(NULL))
    }
    print(out$diagnostics)
  })

  output$download_clusters_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("cluster-summary-", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(computed()$cluster_table, file = file, row.names = FALSE)
    }
  )

  output$signal_plot <- shiny::renderPlot({
    analysis_out <- analyzed_ts_selected()
    ts_tbl <- analysis_out$data
    ids <- selected_cluster_ids()

    shiny::validate(
      shiny::need(nrow(ts_tbl) > 0 && length(ids) > 0,
                   "No clusters available for plotting."),
      shiny::need(input$x_var %in% names(ts_tbl),
                   "Selected x variable is not available.")
    )

    ts_tbl <- ts_tbl[ts_tbl$cluster_id %in% ids, , drop = FALSE]
    if (nrow(ts_tbl) == 0) {
      return(.empty_plot("No selected clusters with signal data"))
    }

    .build_design_plot(
      data = ts_tbl,
      x_var = input$x_var,
      collapse_vars = input$collapse_vars
    )
  })

  output$download_signal_png <- shiny::downloadHandler(
    filename = function() {
      paste0("cluster-signal-", Sys.Date(), ".png")
    },
    content = function(file) {
      ids <- selected_cluster_ids()
      analysis_out <- analyzed_ts_selected()
      ts_tbl <- analysis_out$data

      if (nrow(ts_tbl) == 0 || length(ids) == 0 || !input$x_var %in% names(ts_tbl)) {
        p <- .empty_plot("No data available")
      } else {
        ts_tbl <- ts_tbl[ts_tbl$cluster_id %in% ids, , drop = FALSE]
        p <- .build_design_plot(
          data = ts_tbl,
          x_var = input$x_var,
          collapse_vars = input$collapse_vars
        )
      }

      ggplot2::ggsave(filename = file, plot = p, width = 8, height = 5,
                      dpi = 150)
    }
  )

  output$download_brain_png <- shiny::downloadHandler(
    filename = function() {
      paste0("cluster-brain-", Sys.Date(), ".png")
    },
    content = function(file) {
      dat <- computed()
      overlay_payload <- cluster_overlay_payload()
      overlay_vals <- if (!is.null(overlay_payload)) overlay_payload$overlay else NULL
      surf_ids <- as.integer(surfatlas$ids)
      scope_ids <- brain_scope_ids()

      vals <- .parcel_values_from_clusters(
        cluster_parcels = dat$cluster_parcels,
        atlas_ids = surf_ids,
        selected_cluster_ids = scope_ids,
        mode = input$display_mode
      )

      lim <- NULL
      finite_vals <- vals[is.finite(vals)]
      if (length(finite_vals) > 0) {
        lim_max <- max(abs(finite_vals))
        lim <- c(-lim_max, lim_max)
      }

      p <- .fallback_brain_plot(
        surfatlas = surfatlas,
        vals = vals,
        palette = palette,
        lim = lim,
        interactive = FALSE
      )

      if (isTRUE(use_surface_plot)) {
        p <- tryCatch(
          plot_brain(
            surfatlas = surfatlas,
            vals = vals,
            views = brain_views,
            hemis = brain_hemis,
            palette = palette,
            lim = lim,
            overlay = overlay_vals,
            overlay_threshold = max(abs(input$overlay_threshold), .Machine$double.eps),
            overlay_alpha = input$overlay_alpha,
            overlay_palette = palette,
            interactive = FALSE
          ),
          error = function(e) {
            .fallback_brain_plot(
              surfatlas = surfatlas,
              vals = vals,
              palette = palette,
              lim = lim,
              interactive = FALSE,
              title = paste0("Parcel Layout (Fallback): ", conditionMessage(e))
            )
          }
        )
      }

      ggplot2::ggsave(filename = file, plot = p, width = 8, height = 5,
                      dpi = 150)
    }
  )

  invisible(list(
    computed = computed,
    selected_cluster_ids = selected_cluster_ids,
    sel_state = sel_state,
    analysis_state = analysis_state,
    cluster_ts_selected = cluster_ts_selected,
    analyzed_ts_selected = analyzed_ts_selected
  ))
}

#' Launch Cluster Explorer Shiny App
#'
#' Interactive explorer linking a cluster summary table, parcel brain map, and
#' design-aware signal plots. Clusters are formed volumetrically from
#' \code{stat_map}; visualization is parcel-level on \code{surfatlas}.
#'
#' Calling \code{cluster_explorer()} with no arguments launches a synthetic demo
#' dataset so the UI can be explored immediately.
#'
#' @inheritParams build_cluster_explorer_data
#' @param surfatlas A surface atlas object used by \code{\link{plot_brain}()}.
#'   If \code{NULL}, the function attempts to infer a surface atlas from a
#'   compatible `atlas` input (for example Schaefer or Glasser). If
#'   inference is not possible, and non-demo inputs are otherwise present, input
#'   validation fails.
#' @param design Optional design table (one row per sample). When provided, it
#'   is column-bound to \code{sample_table}.
#' @param overlay_space Optional surface space override used to fetch white/pial
#'   meshes for volumetric cluster projection. Defaults to
#'   \code{surfatlas$surface_space}.
#' @param overlay_density Optional TemplateFlow density override for overlay
#'   surface loading.
#' @param overlay_resolution Optional TemplateFlow resolution override for
#'   overlay surface loading.
#' @param overlay_fun Reduction used by \code{neurosurf::vol_to_surf()}.
#' @param overlay_sampling Sampling strategy for \code{neurosurf::vol_to_surf()}.
#' @param prefetch Logical default for eager cluster signal prefetch.
#' @param prefetch_max_clusters Default max cluster count allowed for prefetch.
#' @param prefetch_max_voxels Default max total cluster voxels allowed for
#'   prefetch.
#' @param palette Continuous palette passed to \code{\link{plot_brain}()}.
#' @param selection_engine Selection backend. \code{"cluster"} uses the
#'   built-in connected-component workflow. \code{"custom"} delegates data
#'   assembly to \code{selection_provider}.
#' @param parcel_ids Optional parcel ids used when
#'   \code{selection_engine = "parcel"}. Default \code{NULL} uses all parcels.
#' @param sphere_centers Optional sphere centers used when
#'   \code{selection_engine = "sphere"}. Provide numeric vector length 3 or
#'   matrix/data.frame with 3 columns.
#' @param sphere_radius Sphere radius used when
#'   \code{selection_engine = "sphere"}.
#' @param sphere_units Units for \code{sphere_centers} and \code{sphere_radius}:
#'   \code{"mm"} (world coordinates) or \code{"voxels"} (grid coordinates).
#' @param sphere_combine How to combine multiple spheres:
#'   \code{"separate"} (one region per sphere) or \code{"union"}.
#' @param selection_provider Optional function used when
#'   \code{selection_engine = "custom"}. Must return a list compatible with
#'   \code{build_cluster_explorer_data()} output.
#' @param analysis_plugins Optional list of analysis plugin definitions. Plugins
#'   can transform extracted time-series/design data before plotting.
#' @param default_analysis_plugin Optional default plugin id. Defaults to
#'   \code{"none"}.
#'
#' @return A \code{shiny.appobj}.
#' @export
cluster_explorer <- function(data_source = NULL,
                             atlas = NULL,
                             stat_map = NULL,
                             surfatlas = NULL,
                             sample_table = NULL,
                             design = NULL,
                             threshold = 3,
                             min_cluster_size = 20,
                             connectivity = c("26-connect",
                                              "18-connect",
                                              "6-connect"),
                             tail = c("two_sided",
                                      "positive",
                                      "negative"),
                             series_fun = NULL,
                             overlay_space = NULL,
                             overlay_density = NULL,
                             overlay_resolution = NULL,
                             overlay_fun = c("avg", "nn", "mode"),
                             overlay_sampling = c("midpoint",
                                                  "normal_line",
                                                  "thickness"),
                             prefetch = TRUE,
                             prefetch_max_clusters = 200,
                             prefetch_max_voxels = 100000,
                             palette = "vik",
                             selection_engine = c("cluster",
                                                  "parcel",
                                                  "sphere",
                                                  "custom"),
                             parcel_ids = NULL,
                             sphere_centers = NULL,
                             sphere_radius = 6,
                             sphere_units = c("mm", "voxels"),
                             sphere_combine = c("separate", "union"),
                             selection_provider = NULL,
                             analysis_plugins = NULL,
                             default_analysis_plugin = "none") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for cluster_explorer().")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for cluster_explorer().")
  }
  if (!requireNamespace("ggiraph", quietly = TRUE)) {
    stop("Package 'ggiraph' is required for cluster_explorer().")
  }

  use_demo_defaults <- is.null(data_source) &&
    is.null(atlas) &&
    is.null(stat_map) &&
    is.null(surfatlas) &&
    is.null(sample_table) &&
    is.null(design)

  if (isTRUE(use_demo_defaults)) {
    demo <- .cluster_explorer_demo_inputs()
    data_source <- demo$data_source
    atlas <- demo$atlas
    stat_map <- demo$stat_map
    surfatlas <- demo$surfatlas
    sample_table <- demo$sample_table
    design <- demo$design
    message("cluster_explorer(): using built-in demo dataset.")
  } else {
    if (is.null(surfatlas) && !is.null(atlas)) {
      inferred <- .infer_surfatlas_from_atlas(atlas)
      if (!is.null(inferred)) {
        surfatlas <- inferred
      }
    }

    missing_required <- c(
      data_source = is.null(data_source),
      atlas = is.null(atlas),
      stat_map = is.null(stat_map),
      surfatlas = is.null(surfatlas)
    )
    if (any(missing_required)) {
      stop(
        "Missing required inputs: ",
        paste(names(missing_required)[missing_required], collapse = ", "),
        ".\nProvide all required inputs, pass a supported `atlas` that can infer a `surfatlas`, ",
        "or call cluster_explorer() with no arguments for demo mode.",
        call. = FALSE
      )
    }
  }

  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.")
  }

  aligned <- .harmonize_cluster_explorer_atlas(atlas, stat_map)
  atlas <- aligned$atlas
  if (!is.null(aligned$message)) {
    message(aligned$message)
  }
  if (!is.null(aligned$warning)) {
    warning(aligned$warning, call. = FALSE)
  }

  .warn_if_atlas_surface_mismatch(atlas, surfatlas)

  selection_engine <- match.arg(selection_engine)
  sphere_units <- match.arg(sphere_units)
  sphere_combine <- match.arg(sphere_combine)
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)
  overlay_fun <- match.arg(overlay_fun)
  overlay_sampling <- match.arg(overlay_sampling)
  plugins <- .normalize_analysis_plugins(
    analysis_plugins = analysis_plugins,
    default_plugin = default_analysis_plugin
  )
  if (!default_analysis_plugin %in% names(plugins)) {
    default_analysis_plugin <- "none"
  }

  n_samples <- .infer_n_samples(
    data_source = data_source,
    sample_table = sample_table,
    design = design
  )
  design_tbl <- .normalize_design_table(design = design, n_samples = n_samples)
  sample_tbl <- .normalize_sample_table(sample_table = sample_table,
                                        n_samples = n_samples)
  sample_tbl <- .merge_sample_and_design(sample_tbl, design_tbl)
  overlay_space_selected <- if (!is.null(overlay_space) && nzchar(overlay_space)) {
    overlay_space
  } else {
    "auto"
  }
  overlay_space_choices <- unique(c(
    "auto",
    "fsaverage6",
    "fsaverage5",
    "fsaverage",
    as.character(overlay_space),
    as.character(surfatlas$surface_space)
  ))
  overlay_space_choices <- overlay_space_choices[
    !is.na(overlay_space_choices) & nzchar(overlay_space_choices)
  ]
  overlay_space_choice_labels <- stats::setNames(
    overlay_space_choices,
    overlay_space_choices
  )
  if ("auto" %in% names(overlay_space_choice_labels)) {
    overlay_space_choice_labels["auto"] <- "Auto (default)"
  }
  analysis_plugin_choices <- vapply(
    plugins,
    function(p) as.character(p$label),
    character(1)
  )
  names(analysis_plugin_choices) <- vapply(
    plugins,
    function(p) as.character(p$id),
    character(1)
  )

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$style(shiny::HTML(.cluster_explorer_css())),
      shiny::tags$script(shiny::HTML(
        "document.addEventListener('DOMContentLoaded', function () {
           var root = document.documentElement;
           var closeDrawer = function () {
             root.classList.remove('ce-analysis-open');
           };
           $(document).on('shiny:busy', function () {
             root.classList.add('ce-busy');
           });
           $(document).on('shiny:idle', function () {
             root.classList.remove('ce-busy');
           });
           if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
             window.Shiny.addCustomMessageHandler('ceAnalysisDrawer', function (msg) {
               if (msg && msg.open) {
                 root.classList.add('ce-analysis-open');
               } else {
                 closeDrawer();
               }
             });
           }
           document.addEventListener('keydown', function (evt) {
             if (evt.key === 'Escape' && root.classList.contains('ce-analysis-open')) {
               closeDrawer();
               if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
                 window.Shiny.setInputValue('analysis_drawer_dismiss', Date.now(), { priority: 'event' });
               }
             }
           });
         });"
      ))
    ),
    shiny::div(
      class = "ce-shell",
      shiny::div(
        class = "ce-topbar",
        shiny::div(
          class = "ce-title-wrap",
          shiny::tags$div(class = "ce-eyebrow", "Neuroatlas"),
          shiny::tags$h1("Cluster and Parcel Activity Explorer"),
          shiny::tags$p(
            class = "ce-subtitle",
            "Volumetric connected components with parcel mapping, projected overlay, and design-aware signal plots."
          )
        ),
        shiny::div(
          class = "ce-actions",
          shiny::actionButton("apply_btn", "Apply", class = "ce-btn ce-btn-primary"),
          shiny::actionButton("reset_filters_btn", "Reset", class = "ce-btn ce-btn-ghost")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::div(
            class = "ce-card ce-controls",
            shiny::tags$details(
              class = "ce-group",
              open = "open",
              shiny::tags$summary("Analysis"),
              shiny::div(
                class = "ce-group-body",
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      "threshold",
                      .ce_label_with_help(
                        "Threshold",
                        "Voxelwise cutoff for cluster formation. Positive and negative handling depends on Tail."
                      ),
                                        value = threshold, step = 0.1)
                  ),
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      "min_cluster_size",
                      .ce_label_with_help(
                        "Min Size",
                        "Minimum number of connected voxels required to keep a cluster."
                      ),
                                        value = min_cluster_size, min = 1, step = 1)
                  )
                ),
                shiny::selectInput("tail",
                                   .ce_label_with_help(
                                     "Tail",
                                     "two_sided: find both positive and negative clusters; positive/negative restricts sign."
                                   ),
                                   choices = c("two_sided", "positive", "negative"),
                                   selected = tail),
                shiny::selectInput("connectivity",
                                   .ce_label_with_help(
                                     "Connectivity",
                                     "Neighborhood definition for connected components: 26 is most permissive, 6 is strictest."
                                   ),
                                   choices = c("26-connect", "18-connect", "6-connect"),
                                   selected = connectivity)
              )
            ),
            shiny::tags$details(
              class = "ce-group",
              shiny::tags$summary("Prefetch"),
              shiny::div(
                class = "ce-group-body",
                shiny::checkboxInput("prefetch_mode", "Prefetch Cluster Signals",
                                     value = prefetch),
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      "prefetch_max_clusters",
                      .ce_label_with_help(
                        "Max Clusters",
                        "Prefetch runs only if total clusters are <= this limit."
                      ),
                                        value = as.integer(prefetch_max_clusters),
                                        min = 1, step = 10)
                  ),
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      "prefetch_max_voxels",
                      .ce_label_with_help(
                        "Max Voxels",
                        "Prefetch runs only if total voxels across clusters are <= this limit."
                      ),
                                        value = as.integer(prefetch_max_voxels),
                                        min = 100, step = 1000)
                  )
                ),
                shiny::div(class = "ce-prefetch-status", shiny::textOutput("prefetch_status"))
              )
            ),
            shiny::tags$details(
              class = "ce-group",
              open = "open",
              shiny::tags$summary("Brain Layer"),
              shiny::div(
                class = "ce-group-body",
                shiny::selectInput(
                  "map_scope",
                  .ce_label_with_help(
                    "Map Scope",
                    "All clusters: brain map uses every detected cluster. Selected clusters only: map reflects current table/brain selection."
                  ),
                  choices = c(
                    "All clusters" = "all_clusters",
                    "Selected clusters only" = "selected_clusters"
                  ),
                                   selected = "all_clusters"),
                shiny::selectInput("display_mode",
                                   .ce_label_with_help(
                                     "Parcel Display",
                                     "dominant shows strongest signed effect per parcel; positive_only/negative_only restrict by sign."
                                   ),
                                   choices = c("dominant", "positive_only",
                                               "negative_only"),
                                   selected = "dominant"),
                shiny::selectInput(
                  "brain_click_mode",
                  .ce_label_with_help(
                    "Brain Click Mode",
                    "Parcel overlap selects clusters by parcel membership. Surface pick maps clicked 2D polygon to a surface node, then to voxel space, and selects nearby clusters."
                  ),
                  choices = c(
                    "Parcel overlap" = "parcel",
                    "Surface pick (sphere)" = "surface_pick"
                  ),
                  selected = "parcel"
                ),
                shiny::numericInput(
                  "surface_pick_radius",
                  .ce_label_with_help(
                    "Surface Pick Radius (voxels)",
                    "Radius around picked voxel center used to match one or more clusters."
                  ),
                  value = 2,
                  min = 0,
                  step = 1
                ),
                shiny::checkboxInput("show_cluster_overlay",
                                     "Projected Cluster Overlay",
                                     value = TRUE),
                shiny::sliderInput("overlay_alpha", "Overlay Alpha",
                                   min = 0, max = 1, value = 0.45, step = 0.05),
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      "overlay_threshold",
                      .ce_label_with_help(
                        "Overlay |v| Threshold",
                        "Projected surface vertices with |value| below this are hidden."
                      ),
                                        value = 1e-06, step = 0.1)
                  ),
                  shiny::column(
                    width = 6,
                    shiny::selectInput(
                      "overlay_fun",
                      .ce_label_with_help(
                        "Overlay Reduction",
                        "How volume samples are reduced at each surface vertex (avg, nearest-neighbor, or mode)."
                      ),
                                       choices = c("avg", "nn", "mode"),
                                       selected = overlay_fun)
                  )
                ),
                shiny::selectInput(
                  "overlay_space_ui",
                  .ce_label_with_help(
                    "Overlay Surface Space",
                    "Default is Auto: use surfatlas surface_space when available, otherwise fallback to fsaverage6."
                  ),
                  choices = overlay_space_choice_labels,
                  selected = overlay_space_selected
                ),
                shiny::selectInput("overlay_sampling",
                                   .ce_label_with_help(
                                     "Overlay Sampling",
                                     "How samples are drawn through cortical depth during volume-to-surface projection."
                                   ),
                                   choices = c("midpoint", "normal_line", "thickness"),
                                   selected = overlay_sampling)
              )
            ),
            shiny::tags$details(
              class = "ce-group",
              shiny::tags$summary("Export"),
              shiny::div(
                class = "ce-group-body ce-export-grid",
                shiny::downloadButton("download_clusters_csv", "Clusters CSV",
                                      class = "ce-btn ce-btn-ghost ce-btn-block"),
                shiny::downloadButton("download_signal_png", "Signal PNG",
                                      class = "ce-btn ce-btn-ghost ce-btn-block"),
                shiny::downloadButton("download_brain_png", "Brain PNG",
                                      class = "ce-btn ce-btn-ghost ce-btn-block")
              )
            ),
            shiny::div(
              class = "ce-help",
              paste0(
                "Surface space: ",
                if (!is.null(surfatlas$surface_space)) surfatlas$surface_space else "unknown",
                ". Overlay projection default: Auto (surfatlas surface_space, fallback fsaverage6). ",
                "Keep stat_map and template surfaces in compatible world coordinates (MNI152NLin2009* recommended)."
              )
            )
          )
        ),
        shiny::column(
          width = 9,
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::div(
                class = "ce-card ce-panel",
                shiny::tags$div(class = "ce-panel-head", "Brain View"),
                shiny::div(
                  class = "ce-output-shell ce-output-brain",
                  ggiraph::girafeOutput("brain_plot", height = "430px")
                )
              )
            ),
            shiny::column(
              width = 6,
              shiny::div(
                class = "ce-card ce-panel",
                shiny::div(
                  class = "ce-panel-head ce-panel-head-with-action",
                  shiny::tags$span("Signal View"),
                  shiny::actionButton(
                    "analysis_open_btn",
                    "Analysis",
                    class = "ce-btn ce-btn-ghost ce-btn-xs"
                  )
                ),
                shiny::div(
                  class = "ce-plot-controls",
                  shiny::div(
                    class = "ce-plot-field",
                    shiny::selectInput(
                      "x_var",
                      .ce_label_with_help(
                        "Plot Variable",
                        "X-axis variable for cluster signal plotting."
                      ),
                                       choices = ".sample_index",
                                       selected = ".sample_index")
                  ),
                  shiny::div(
                    class = "ce-plot-field",
                    shiny::selectizeInput(
                      "collapse_vars",
                      .ce_label_with_help(
                        "Collapse By",
                        "Average signal within combinations of selected variables before plotting."
                      ),
                                          choices = character(0),
                                          multiple = TRUE)
                  ),
                  shiny::div(
                    class = "ce-plot-field ce-plot-field-btn",
                    shiny::actionButton("replot_table_btn", "Use Table Selection",
                                        class = "ce-btn ce-btn-ghost ce-btn-block")
                  )
                ),
                shiny::div(
                  class = "ce-output-shell ce-output-signal",
                  shiny::plotOutput("signal_plot", height = "430px")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "ce-card ce-panel",
                shiny::tags$div(class = "ce-panel-head", "Cluster Table"),
                shiny::div(
                  class = "ce-output-shell ce-output-table",
                  DT::DTOutput("cluster_table")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                class = "ce-card ce-panel",
                shiny::tags$details(
                  shiny::tags$summary("Overlay Diagnostics"),
                  shiny::verbatimTextOutput("overlay_diag")
                ),
                shiny::tags$details(
                  shiny::tags$summary("Analysis Diagnostics"),
                  shiny::verbatimTextOutput("analysis_diag")
                )
              )
            )
          )
        )
      ),
      shiny::tags$div(
        class = "ce-analysis-overlay",
        onclick = "if (window.Shiny && typeof window.Shiny.setInputValue === 'function') { window.Shiny.setInputValue('analysis_drawer_dismiss', Date.now(), { priority: 'event' }); }"
      ),
      shiny::tags$aside(
        class = "ce-analysis-drawer",
        shiny::div(
          class = "ce-analysis-drawer-head",
          shiny::tags$div(
            class = "ce-analysis-drawer-title",
            shiny::tags$div(class = "ce-eyebrow", "Signal Analysis"),
            shiny::tags$h2("Analysis Plugin")
          ),
          shiny::actionButton(
            "analysis_close_btn",
            "Close",
            class = "ce-btn ce-btn-ghost ce-btn-xs"
          )
        ),
        shiny::div(
          class = "ce-analysis-drawer-body",
          shiny::selectInput(
            "analysis_plugin_id",
            .ce_label_with_help(
              "Analysis Plugin",
              "Optional transform/fit stage applied to extracted time-series before plotting."
            ),
            choices = analysis_plugin_choices,
            selected = default_analysis_plugin
          ),
          shiny::uiOutput("analysis_params_ui"),
          shiny::actionButton(
            "analysis_apply_btn",
            "Apply Analysis",
            class = "ce-btn ce-btn-primary ce-btn-block"
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    .cluster_explorer_server(
      input = input,
      output = output,
      session = session,
      surfatlas = surfatlas,
      stat_map = stat_map,
      data_source = data_source,
      atlas = atlas,
      sample_tbl = sample_tbl,
      series_fun = series_fun,
      selection_engine = selection_engine,
      selection_provider = selection_provider,
      parcel_ids = parcel_ids,
      sphere_centers = sphere_centers,
      sphere_radius = sphere_radius,
      sphere_units = sphere_units,
      sphere_combine = sphere_combine,
      plugins = plugins,
      default_analysis_plugin = default_analysis_plugin,
      overlay_space = overlay_space,
      overlay_density = overlay_density,
      overlay_resolution = overlay_resolution,
      palette = palette,
      threshold = threshold,
      min_cluster_size = min_cluster_size
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Launch Cluster Explorer in Interactive Session
#'
#' Convenience wrapper around \code{\link{cluster_explorer}()} that launches the
#' app in an interactive session.
#'
#' @param ... Passed to \code{\link{cluster_explorer}()}.
#' @return Invisibly returns the running app object.
#' @export
launch_cluster_explorer <- function(...) {
  app <- cluster_explorer(...)
  shiny::runApp(app)
  invisible(app)
}

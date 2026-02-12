#' Build Cluster Explorer Data
#'
#' Compute sign-aware volumetric connected components from a statistic map,
#' annotate them with atlas parcels, and extract cluster-level 4D signal
#' summaries aligned to sample rows.
#'
#' @param data_source A sample-wise data source supporting
#'   \code{neuroim2::series(data_source, i)} where \code{i} is voxel coordinates
#'   (\code{n x 3} matrix) or indices. Rows of the returned matrix correspond to
#'   samples/design rows.
#' @param atlas A volumetric \code{atlas} object used for parcel annotation.
#' @param stat_map A \code{NeuroVol} statistic image used for thresholding and
#'   clustering.
#' @param sample_table Optional data frame with one row per sample. If
#'   \code{NULL}, a default table with \code{.sample_index} is created.
#' @param threshold Numeric threshold used for cluster formation.
#' @param min_cluster_size Minimum number of voxels required to keep a cluster.
#' @param connectivity Connectivity passed to \code{neuroim2::conn_comp()}.
#' @param tail Clustering mode: \code{"positive"}, \code{"negative"}, or
#'   \code{"two_sided"}.
#' @param signal_fun Function used to summarize cluster signal per sample.
#' @param signal_fun_args Named list of additional arguments passed to
#'   \code{signal_fun}.
#' @param series_fun Optional function override for extracting voxel-wise sample
#'   series. Must accept \code{(data_source, i)} and return a matrix-like object
#'   with one row per sample.
#' @param prefetch Logical; if \code{TRUE}, precompute signal summaries for all
#'   clusters on recompute.
#' @param prefetch_max_clusters Maximum clusters allowed for eager prefetch.
#'   Prefetch is skipped when exceeded.
#' @param prefetch_max_voxels Maximum total cluster voxels allowed for eager
#'   prefetch. Prefetch is skipped when exceeded.
#' @param series_cache_env Optional environment used to memoize
#'   voxel-coordinates-to-series extraction across cluster computations.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{cluster_table}}{Cluster summary tibble.}
#'   \item{\code{cluster_parcels}}{Cluster-parcel overlap tibble.}
#'   \item{\code{cluster_ts}}{Sample-level cluster signal tibble.}
#'   \item{\code{cluster_voxels}}{Named list of voxel coordinate matrices by
#'     cluster ID.}
#'   \item{\code{cluster_index}}{A \code{NeuroVol} of global cluster IDs.}
#'   \item{\code{sample_table}}{Normalized sample table with
#'     \code{.sample_index}.}
#'   \item{\code{prefetch_info}}{List describing whether eager signal prefetch
#'     was requested/applied and the effective guard thresholds.}
#' }
#' @export
build_cluster_explorer_data <- function(data_source,
                                        atlas,
                                        stat_map,
                                        sample_table = NULL,
                                        threshold = 3,
                                        min_cluster_size = 20,
                                        connectivity = c("26-connect",
                                                         "18-connect",
                                                         "6-connect"),
                                        tail = c("two_sided",
                                                 "positive",
                                                 "negative"),
                                        signal_fun = mean,
                                        signal_fun_args = list(na.rm = TRUE),
                                        series_fun = NULL,
                                        prefetch = TRUE,
                                        prefetch_max_clusters = Inf,
                                        prefetch_max_voxels = Inf,
                                        series_cache_env = NULL) {
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)
  n_samples <- .infer_n_samples(
    data_source = data_source,
    sample_table = sample_table,
    design = NULL
  )

  .validate_cluster_explorer_inputs(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    n_samples = n_samples,
    series_fun = series_fun
  )

  sample_tbl <- .normalize_sample_table(
    sample_table = sample_table,
    n_samples = n_samples
  )

  comp <- .extract_stat_clusters(
    stat_map = stat_map,
    threshold = threshold,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity,
    tail = tail
  )

  ann <- .annotate_clusters_with_atlas(
    cluster_table = comp$cluster_table,
    cluster_voxels = comp$cluster_voxels,
    atlas = atlas,
    stat_map = stat_map
  )

  n_clusters <- nrow(ann$cluster_table)
  total_voxels <- if (n_clusters > 0) sum(ann$cluster_table$n_voxels) else 0
  prefetch_allowed <- isTRUE(prefetch) &&
    (n_clusters <= prefetch_max_clusters) &&
    (total_voxels <= prefetch_max_voxels)

  if (isTRUE(prefetch_allowed)) {
    ts_tbl <- .compute_cluster_timeseries(
      data_source = data_source,
      cluster_voxels = comp$cluster_voxels,
      sample_table = sample_tbl,
      series_fun = series_fun,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_cache_env = series_cache_env
    )
  } else {
    ts_tbl <- tibble::tibble()
  }

  if (nrow(ts_tbl) > 0 && nrow(ann$cluster_table) > 0) {
    ts_tbl <- dplyr::left_join(
      ts_tbl,
      ann$cluster_table[, c("cluster_id", "sign"), drop = FALSE],
      by = "cluster_id"
    )
  }

  list(
    cluster_table = ann$cluster_table,
    cluster_parcels = ann$cluster_parcels,
    cluster_ts = ts_tbl,
    cluster_voxels = comp$cluster_voxels,
    cluster_index = comp$cluster_index,
    sample_table = sample_tbl,
    prefetch_info = list(
      requested = isTRUE(prefetch),
      applied = isTRUE(prefetch_allowed),
      n_clusters = n_clusters,
      total_voxels = total_voxels,
      max_clusters = prefetch_max_clusters,
      max_voxels = prefetch_max_voxels
    )
  )
}

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
#'   If \code{NULL} and all primary data inputs are also \code{NULL}, a bundled
#'   synthetic demo dataset is used.
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
        ".\nProvide all required inputs, or call cluster_explorer() with no arguments for demo mode.",
        call. = FALSE
      )
    }
  }

  if (!inherits(surfatlas, "surfatlas")) {
    stop("'surfatlas' must inherit from class 'surfatlas'.")
  }

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
        threshold = input$threshold,
        min_cluster_size = as.integer(input$min_cluster_size),
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
            threshold = max(abs(input$overlay_threshold), .Machine$double.eps),
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

      if (nrow(ts_tbl) == 0 || length(ids) == 0) {
        return(.empty_plot("No clusters available for plotting"))
      }

      ts_tbl <- ts_tbl[ts_tbl$cluster_id %in% ids, , drop = FALSE]
      if (nrow(ts_tbl) == 0) {
        return(.empty_plot("No selected clusters with signal data"))
      }

      if (!input$x_var %in% names(ts_tbl)) {
        return(.empty_plot("Selected x variable is not available"))
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

#' Infer Design Variable Type
#'
#' Classify a vector as \code{"continuous"} or \code{"categorical"} for plot
#' defaults.
#'
#' @param x A vector.
#' @return A character scalar.
#' @export
infer_design_var_type <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return("continuous")
  }
  if (is.numeric(x) || is.integer(x) || is.double(x)) {
    return("continuous")
  }
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return("categorical")
  }
  "categorical"
}

.compute_selection_data <- function(selection_engine = c("cluster",
                                                         "parcel",
                                                         "sphere",
                                                         "custom"),
                                    selection_provider = NULL,
                                    data_source,
                                    atlas,
                                    stat_map,
                                    sample_table = NULL,
                                    threshold = 3,
                                    min_cluster_size = 20,
                                    connectivity = c("26-connect",
                                                     "18-connect",
                                                     "6-connect"),
                                    tail = c("two_sided",
                                             "positive",
                                             "negative"),
                                    signal_fun = mean,
                                    signal_fun_args = list(na.rm = TRUE),
                                    series_fun = NULL,
                                    prefetch = TRUE,
                                    prefetch_max_clusters = Inf,
                                    prefetch_max_voxels = Inf,
                                    series_cache_env = NULL,
                                    parcel_ids = NULL,
                                    sphere_centers = NULL,
                                    sphere_radius = 6,
                                    sphere_units = c("mm", "voxels"),
                                    sphere_combine = c("separate", "union")) {
  selection_engine <- match.arg(selection_engine)
  connectivity <- match.arg(connectivity)
  tail <- match.arg(tail)
  sphere_units <- match.arg(sphere_units)
  sphere_combine <- match.arg(sphere_combine)

  if (identical(selection_engine, "cluster")) {
    return(build_cluster_explorer_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      connectivity = connectivity,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (identical(selection_engine, "parcel")) {
    return(.build_parcel_selection_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      parcel_ids = parcel_ids,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (identical(selection_engine, "sphere")) {
    return(.build_sphere_selection_data(
      data_source = data_source,
      atlas = atlas,
      stat_map = stat_map,
      sample_table = sample_table,
      sphere_centers = sphere_centers,
      sphere_radius = sphere_radius,
      sphere_units = sphere_units,
      sphere_combine = sphere_combine,
      threshold = threshold,
      min_cluster_size = min_cluster_size,
      tail = tail,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_fun = series_fun,
      prefetch = prefetch,
      prefetch_max_clusters = prefetch_max_clusters,
      prefetch_max_voxels = prefetch_max_voxels,
      series_cache_env = series_cache_env
    ))
  }

  if (!is.function(selection_provider)) {
    stop(
      "selection_provider must be a function when selection_engine = 'custom'.",
      call. = FALSE
    )
  }

  out <- selection_provider(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    threshold = threshold,
    min_cluster_size = min_cluster_size,
    connectivity = connectivity,
    tail = tail,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env,
    parcel_ids = parcel_ids,
    sphere_centers = sphere_centers,
    sphere_radius = sphere_radius,
    sphere_units = sphere_units,
    sphere_combine = sphere_combine
  )
  required <- c(
    "cluster_table", "cluster_parcels", "cluster_ts", "cluster_voxels",
    "cluster_index", "sample_table", "prefetch_info"
  )
  miss <- setdiff(required, names(out))
  if (length(miss) > 0) {
    stop(
      "selection_provider result missing required fields: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  out
}

.build_parcel_selection_data <- function(data_source,
                                         atlas,
                                         stat_map,
                                         sample_table = NULL,
                                         parcel_ids = NULL,
                                         threshold = 3,
                                         min_cluster_size = 20,
                                         tail = c("two_sided",
                                                  "positive",
                                                  "negative"),
                                         signal_fun = mean,
                                         signal_fun_args = list(na.rm = TRUE),
                                         series_fun = NULL,
                                         prefetch = TRUE,
                                         prefetch_max_clusters = Inf,
                                         prefetch_max_voxels = Inf,
                                         series_cache_env = NULL) {
  tail <- match.arg(tail)
  atlas_arr <- .atlas_volume_array(.get_atlas_volume(atlas))
  stat_arr <- as.array(stat_map)

  ids_all <- sort(unique(as.integer(atlas_arr[atlas_arr != 0])))
  if (is.null(parcel_ids)) {
    ids <- ids_all
  } else {
    ids <- intersect(ids_all, as.integer(parcel_ids))
  }

  cluster_voxels <- list()
  cluster_sign <- character(0)
  for (pid in ids) {
    idx <- which(atlas_arr == pid)
    if (length(idx) == 0) next

    vox <- as.matrix(arrayInd(idx, .dim = dim(atlas_arr)))
    if (nrow(vox) < min_cluster_size) next

    svals <- as.numeric(stat_arr[idx])
    peak <- svals[which.max(abs(svals))]
    if (!.keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
      next
    }

    cid <- paste0("R", pid)
    cluster_voxels[[cid]] <- vox
    cluster_sign[[cid]] <- .sign_label_from_peak(peak)
  }

  .build_selection_data_from_voxels(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    cluster_voxels = cluster_voxels,
    cluster_sign = cluster_sign,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env
  )
}

.build_sphere_selection_data <- function(data_source,
                                         atlas,
                                         stat_map,
                                         sample_table = NULL,
                                         sphere_centers = NULL,
                                         sphere_radius = 6,
                                         sphere_units = c("mm", "voxels"),
                                         sphere_combine = c("separate", "union"),
                                         threshold = 3,
                                         min_cluster_size = 20,
                                         tail = c("two_sided",
                                                  "positive",
                                                  "negative"),
                                         signal_fun = mean,
                                         signal_fun_args = list(na.rm = TRUE),
                                         series_fun = NULL,
                                         prefetch = TRUE,
                                         prefetch_max_clusters = Inf,
                                         prefetch_max_voxels = Inf,
                                         series_cache_env = NULL) {
  tail <- match.arg(tail)
  sphere_units <- match.arg(sphere_units)
  sphere_combine <- match.arg(sphere_combine)

  assertthat::assert_that(
    is.numeric(sphere_radius) && length(sphere_radius) == 1 &&
      is.finite(sphere_radius) && sphere_radius > 0,
    msg = "'sphere_radius' must be a positive numeric scalar."
  )

  dims3 <- dim(stat_map)[1:3]
  stat_arr <- as.array(stat_map)
  all_idx <- seq_len(prod(dims3))
  grid_all <- as.matrix(arrayInd(all_idx, .dim = dims3))
  coord_all <- if (identical(sphere_units, "mm")) {
    neuroim2::grid_to_coord(neuroim2::space(stat_map), grid_all)
  } else {
    grid_all
  }

  centers <- .normalize_sphere_centers(sphere_centers)
  if (is.null(centers)) {
    peak_idx <- which.max(abs(stat_arr))
    peak_grid <- as.matrix(arrayInd(peak_idx, .dim = dims3))
    centers <- if (identical(sphere_units, "mm")) {
      neuroim2::grid_to_coord(neuroim2::space(stat_map), peak_grid)
    } else {
      peak_grid
    }
  }

  cluster_voxels <- list()
  cluster_sign <- character(0)

  if (identical(sphere_combine, "union")) {
    keep <- rep(FALSE, nrow(coord_all))
    for (i in seq_len(nrow(centers))) {
      d2 <- rowSums((coord_all - matrix(centers[i, ], nrow(coord_all), 3,
                                        byrow = TRUE))^2)
      keep <- keep | (d2 <= sphere_radius^2)
    }
    idx <- which(keep)
    if (length(idx) > 0) {
      vox <- grid_all[idx, , drop = FALSE]
      svals <- as.numeric(stat_arr[idx])
      peak <- svals[which.max(abs(svals))]
      if (nrow(vox) >= min_cluster_size &&
          .keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
        cluster_voxels[["S1"]] <- vox
        cluster_sign[["S1"]] <- .sign_label_from_peak(peak)
      }
    }
  } else {
    for (i in seq_len(nrow(centers))) {
      d2 <- rowSums((coord_all - matrix(centers[i, ], nrow(coord_all), 3,
                                        byrow = TRUE))^2)
      idx <- which(d2 <= sphere_radius^2)
      if (length(idx) == 0) next
      vox <- grid_all[idx, , drop = FALSE]
      if (nrow(vox) < min_cluster_size) next

      svals <- as.numeric(stat_arr[idx])
      peak <- svals[which.max(abs(svals))]
      if (!.keep_peak_for_tail(peak_stat = peak, threshold = threshold, tail = tail)) {
        next
      }
      cid <- paste0("S", i)
      cluster_voxels[[cid]] <- vox
      cluster_sign[[cid]] <- .sign_label_from_peak(peak)
    }
  }

  .build_selection_data_from_voxels(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    cluster_voxels = cluster_voxels,
    cluster_sign = cluster_sign,
    signal_fun = signal_fun,
    signal_fun_args = signal_fun_args,
    series_fun = series_fun,
    prefetch = prefetch,
    prefetch_max_clusters = prefetch_max_clusters,
    prefetch_max_voxels = prefetch_max_voxels,
    series_cache_env = series_cache_env
  )
}

.build_selection_data_from_voxels <- function(data_source,
                                              atlas,
                                              stat_map,
                                              sample_table = NULL,
                                              cluster_voxels,
                                              cluster_sign = character(0),
                                              signal_fun = mean,
                                              signal_fun_args = list(na.rm = TRUE),
                                              series_fun = NULL,
                                              prefetch = TRUE,
                                              prefetch_max_clusters = Inf,
                                              prefetch_max_voxels = Inf,
                                              series_cache_env = NULL) {
  n_samples <- .infer_n_samples(
    data_source = data_source,
    sample_table = sample_table,
    design = NULL
  )
  .validate_cluster_explorer_inputs(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    sample_table = sample_table,
    n_samples = n_samples,
    series_fun = series_fun
  )
  sample_tbl <- .normalize_sample_table(sample_table = sample_table,
                                        n_samples = n_samples)

  stat_arr <- as.array(stat_map)
  dims3 <- dim(stat_arr)[1:3]
  index_arr <- array(0L, dim = dims3)

  rows <- list()
  clean_vox <- list()
  k <- 0L
  ids <- names(cluster_voxels)
  if (is.null(ids)) ids <- paste0("K", seq_along(cluster_voxels))

  for (i in seq_along(cluster_voxels)) {
    vox <- as.matrix(cluster_voxels[[i]])
    if (is.null(vox) || nrow(vox) == 0) next
    if (ncol(vox) != 3) next

    vox <- matrix(as.integer(vox), ncol = 3)
    vox <- vox[
      vox[, 1] >= 1 & vox[, 1] <= dims3[1] &
        vox[, 2] >= 1 & vox[, 2] <= dims3[2] &
        vox[, 3] >= 1 & vox[, 3] <= dims3[3],
      , drop = FALSE
    ]
    if (nrow(vox) == 0) next
    vox <- unique(vox)

    idx <- .grid_to_linear_index(vox, dims3)
    svals <- as.numeric(stat_arr[idx])
    peak_i <- which.max(abs(svals))
    peak <- vox[peak_i, ]
    peak_stat <- svals[peak_i]

    k <- k + 1L
    cid <- as.character(ids[i])
    clean_vox[[cid]] <- vox
    index_arr[idx] <- k

    sign_val <- cluster_sign[[cid]]
    if (is.null(sign_val) || !is.character(sign_val) ||
        length(sign_val) == 0 || is.na(sign_val) || !nzchar(sign_val[1])) {
      sign_val <- .sign_label_from_peak(peak_stat)
    }

    rows[[length(rows) + 1L]] <- tibble::tibble(
      cluster_id = cid,
      sign = sign_val,
      component_id = k,
      n_voxels = nrow(vox),
      peak_x = as.integer(peak[1]),
      peak_y = as.integer(peak[2]),
      peak_z = as.integer(peak[3]),
      max_stat = as.numeric(peak_stat),
      peak_coord = paste0("(", peak[1], ", ", peak[2], ", ", peak[3], ")")
    )
  }

  cluster_tbl <- if (length(rows) == 0) {
    tibble::tibble(
      cluster_id = character(0),
      sign = character(0),
      component_id = integer(0),
      n_voxels = integer(0),
      peak_x = integer(0),
      peak_y = integer(0),
      peak_z = integer(0),
      max_stat = numeric(0),
      peak_coord = character(0)
    )
  } else {
    dplyr::bind_rows(rows)
  }

  ann <- .annotate_clusters_with_atlas(
    cluster_table = cluster_tbl,
    cluster_voxels = clean_vox,
    atlas = atlas,
    stat_map = stat_map
  )

  n_clusters <- nrow(ann$cluster_table)
  total_voxels <- if (n_clusters > 0) sum(ann$cluster_table$n_voxels) else 0
  prefetch_allowed <- isTRUE(prefetch) &&
    (n_clusters <= prefetch_max_clusters) &&
    (total_voxels <= prefetch_max_voxels)

  if (isTRUE(prefetch_allowed)) {
    ts_tbl <- .compute_cluster_timeseries(
      data_source = data_source,
      cluster_voxels = clean_vox,
      sample_table = sample_tbl,
      series_fun = series_fun,
      signal_fun = signal_fun,
      signal_fun_args = signal_fun_args,
      series_cache_env = series_cache_env
    )
  } else {
    ts_tbl <- tibble::tibble()
  }

  if (nrow(ts_tbl) > 0 && nrow(ann$cluster_table) > 0) {
    ts_tbl <- dplyr::left_join(
      ts_tbl,
      ann$cluster_table[, c("cluster_id", "sign"), drop = FALSE],
      by = "cluster_id"
    )
  }

  list(
    cluster_table = ann$cluster_table,
    cluster_parcels = ann$cluster_parcels,
    cluster_ts = ts_tbl,
    cluster_voxels = clean_vox,
    cluster_index = neuroim2::NeuroVol(index_arr, space = neuroim2::space(stat_map)),
    sample_table = sample_tbl,
    prefetch_info = list(
      requested = isTRUE(prefetch),
      applied = isTRUE(prefetch_allowed),
      n_clusters = n_clusters,
      total_voxels = total_voxels,
      max_clusters = prefetch_max_clusters,
      max_voxels = prefetch_max_voxels
    )
  )
}

.normalize_sphere_centers <- function(sphere_centers) {
  if (is.null(sphere_centers)) return(NULL)
  if (is.numeric(sphere_centers) && length(sphere_centers) == 3) {
    return(matrix(as.numeric(sphere_centers), nrow = 1))
  }
  if (is.data.frame(sphere_centers)) {
    sphere_centers <- as.matrix(sphere_centers)
  }
  if (is.matrix(sphere_centers) && ncol(sphere_centers) == 3) {
    storage.mode(sphere_centers) <- "double"
    return(sphere_centers)
  }
  stop(
    "'sphere_centers' must be NULL, a numeric length-3 vector, ",
    "or an n x 3 matrix/data.frame.",
    call. = FALSE
  )
}

.keep_peak_for_tail <- function(peak_stat, threshold, tail) {
  if (!is.finite(peak_stat)) return(FALSE)
  if (identical(tail, "positive")) {
    return(peak_stat > threshold)
  }
  if (identical(tail, "negative")) {
    return(peak_stat < -threshold)
  }
  abs(peak_stat) > threshold
}

.sign_label_from_peak <- function(peak_stat) {
  if (!is.finite(peak_stat) || peak_stat == 0) return("unsigned")
  if (peak_stat > 0) "positive" else "negative"
}

.as_analysis_plugin <- function(x, fallback_id = "plugin") {
  if (is.null(x)) return(NULL)

  if (is.function(x)) {
    return(list(
      id = fallback_id,
      label = fallback_id,
      run = x,
      param_defs = list()
    ))
  }

  if (is.list(x)) {
    if (!is.function(x$run)) {
      stop("Analysis plugin must define a callable '$run' function.",
           call. = FALSE)
    }
    id <- if (!is.null(x$id) && nzchar(x$id)) as.character(x$id) else fallback_id
    label <- if (!is.null(x$label) && nzchar(x$label)) {
      as.character(x$label)
    } else {
      id
    }
    defs <- x$param_defs
    if (is.null(defs)) defs <- list()
    return(list(
      id = id,
      label = label,
      run = x$run,
      param_defs = defs
    ))
  }

  stop("Analysis plugins must be functions or lists with fields id/label/run.",
       call. = FALSE)
}

.normalize_analysis_plugins <- function(analysis_plugins = NULL,
                                        default_plugin = "none") {
  none_plugin <- list(
    id = "none",
    label = "None (raw signal)",
    run = function(ts_data, design, params = list(), context = list()) {
      list(data = ts_data, design = design, diagnostics = NULL, meta = list())
    },
    param_defs = list()
  )

  plugs <- list(none = none_plugin)
  if (!is.null(analysis_plugins)) {
    if (!is.list(analysis_plugins)) {
      analysis_plugins <- list(analysis_plugins)
    }
    nm <- names(analysis_plugins)
    for (i in seq_along(analysis_plugins)) {
      fallback_id <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else {
        paste0("plugin", i)
      }
      p <- .as_analysis_plugin(analysis_plugins[[i]], fallback_id = fallback_id)
      plugs[[p$id]] <- p
    }
  }

  if (!default_plugin %in% names(plugs)) {
    default_plugin <- "none"
  }

  plugs <- c(plugs[setdiff(names(plugs), default_plugin)],
             plugs[default_plugin])
  plugs[c(default_plugin, setdiff(names(plugs), default_plugin))]
}

.analysis_plugin_param_ui <- function(plugin) {
  val_or <- function(x, y) if (is.null(x)) y else x

  defs <- plugin$param_defs
  if (length(defs) == 0) {
    return(shiny::tags$div(class = "ce-help", "No plugin parameters."))
  }

  controls <- lapply(defs, function(def) {
    type <- if (!is.null(def$type)) as.character(def$type) else "numeric"
    name <- as.character(def$name)
    label <- if (!is.null(def$label)) as.character(def$label) else name
    if (!is.null(def$help) && nzchar(as.character(def$help))) {
      label <- .ce_label_with_help(label, as.character(def$help))
    }
    input_id <- paste0("analysis_param_", name)
    value <- def$default

    switch(
      type,
      numeric = shiny::numericInput(
        input_id, label, value = as.numeric(val_or(value, 0)),
        min = if (!is.null(def$min)) as.numeric(def$min) else NA_real_,
        max = if (!is.null(def$max)) as.numeric(def$max) else NA_real_,
        step = if (!is.null(def$step)) as.numeric(def$step) else NA_real_
      ),
      integer = shiny::numericInput(
        input_id, label, value = as.integer(val_or(value, 0L)),
        min = if (!is.null(def$min)) as.integer(def$min) else NA_integer_,
        max = if (!is.null(def$max)) as.integer(def$max) else NA_integer_,
        step = if (!is.null(def$step)) as.integer(def$step) else 1L
      ),
      logical = shiny::checkboxInput(
        input_id, label, value = isTRUE(value)
      ),
      text = shiny::textInput(
        input_id, label, value = as.character(val_or(value, ""))
      ),
      select = shiny::selectInput(
        input_id, label,
        choices = val_or(def$choices, character(0)),
        selected = value
      ),
      shiny::textInput(
        input_id, label, value = as.character(val_or(value, ""))
      )
    )
  })

  shiny::tagList(controls)
}

.collect_analysis_params <- function(input, plugin) {
  defs <- plugin$param_defs
  if (length(defs) == 0) return(list())

  params <- lapply(defs, function(def) {
    input_id <- paste0("analysis_param_", as.character(def$name))
    val <- input[[input_id]]
    if (is.null(val) && !is.null(def$default)) {
      val <- def$default
    }
    val
  })
  names(params) <- vapply(defs, function(def) as.character(def$name), character(1))
  params
}

.run_analysis_plugin <- function(plugin,
                                 ts_data,
                                 design,
                                 params = list(),
                                 context = list()) {
  if (nrow(ts_data) == 0 || is.null(plugin) || identical(plugin$id, "none")) {
    return(list(data = ts_data, design = design, diagnostics = NULL, meta = list()))
  }

  raw <- tryCatch(
    plugin$run(ts_data = ts_data, design = design, params = params,
               context = context),
    error = function(e) {
      list(
        data = ts_data,
        design = design,
        diagnostics = list(
          status = "error",
          reason = conditionMessage(e),
          plugin = plugin$id
        ),
        meta = list(plugin_id = plugin$id, failed = TRUE)
      )
    }
  )

  if (is.data.frame(raw)) {
    raw <- list(data = tibble::as_tibble(raw), design = design,
                diagnostics = NULL, meta = list(plugin_id = plugin$id))
  }

  if (!is.list(raw)) {
    return(list(
      data = ts_data,
      design = design,
      diagnostics = list(
        status = "error",
        reason = "Plugin returned unsupported output type."
      ),
      meta = list(plugin_id = plugin$id, failed = TRUE)
    ))
  }

  if (is.null(raw$data) || !is.data.frame(raw$data)) {
    raw$data <- ts_data
  } else {
    raw$data <- tibble::as_tibble(raw$data)
  }
  if (is.null(raw$design) || !is.data.frame(raw$design)) {
    raw$design <- design
  } else {
    raw$design <- tibble::as_tibble(raw$design)
  }
  if (is.null(raw$meta)) raw$meta <- list()
  raw$meta$plugin_id <- plugin$id
  raw
}

.cluster_explorer_demo_inputs <- function(n_time = 48L) {
  dims <- c(12L, 12L, 12L)
  sp3 <- neuroim2::NeuroSpace(
    dim = dims,
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[2:4, 2:4, 2:4] <- 1L
  atlas_arr[8:10, 2:4, 2:4] <- 2L
  atlas_arr[2:4, 8:10, 2:4] <- 3L
  atlas_arr[8:10, 8:10, 2:4] <- 4L
  atlas_arr[2:4, 5:7, 8:10] <- 5L
  atlas_arr[8:10, 5:7, 8:10] <- 6L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas <- list(
    name = "demo_atlas",
    atlas = atlas_vol,
    ids = 1:6,
    labels = c("Frontal-A", "Frontal-B", "Parietal-A",
               "Parietal-B", "Temporal-A", "Temporal-B"),
    orig_labels = c("Frontal-A", "Frontal-B", "Parietal-A",
                    "Parietal-B", "Temporal-A", "Temporal-B"),
    hemi = c("left", "right", "left", "right", "left", "right"),
    roi_metadata = tibble::tibble(
      id = 1:6,
      label = c("Frontal-A", "Frontal-B", "Parietal-A",
                "Parietal-B", "Temporal-A", "Temporal-B"),
      hemi = c("left", "right", "left", "right", "left", "right")
    )
  )
  class(atlas) <- c("demo", "atlas")

  stat_arr <- array(0, dim = dims)
  stat_arr[2:4, 2:4, 2:4] <- 4.8
  stat_arr[8:10, 5:7, 8:10] <- -5.1
  stat_arr[6, 6, 6] <- 8.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp3)

  sp4 <- neuroim2::NeuroSpace(
    dim = c(dims, n_time),
    spacing = c(2, 2, 2),
    origin = c(-12, -12, -12)
  )

  data_arr <- array(0, dim = c(dims, n_time))
  p_mask <- atlas_arr == 1L
  n_mask <- atlas_arr == 6L
  bg_mask <- atlas_arr %in% c(2L, 3L, 4L, 5L)

  t_idx <- seq_len(n_time)
  p_sig <- 0.5 + 0.7 * sin(t_idx / 5)
  n_sig <- 1.2 + 0.55 * cos(t_idx / 7)
  bg_sig <- 0.2 + 0.15 * sin(t_idx / 9)

  for (t in t_idx) {
    vol_t <- data_arr[,,, t]
    vol_t[p_mask] <- p_sig[t]
    vol_t[n_mask] <- n_sig[t]
    vol_t[bg_mask] <- bg_sig[t]
    data_arr[,,, t] <- vol_t
  }

  data_source <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    sample_id = sprintf("sample_%03d", t_idx),
    time = t_idx,
    run = factor(ifelse(t_idx <= n_time / 2, "run1", "run2")),
    condition = factor(rep(c("A", "B", "C"), length.out = n_time))
  )

  design <- tibble::tibble(
    task_load = as.numeric(scale(t_idx)),
    cue = factor(rep(c("cue", "probe"), length.out = n_time))
  )

  surfatlas <- list(
    name = "demo_surfatlas",
    ids = atlas$ids,
    labels = atlas$labels,
    hemi = atlas$hemi
  )
  class(surfatlas) <- c("demo_surfatlas", "surfatlas", "atlas")

  list(
    data_source = data_source,
    atlas = atlas,
    stat_map = stat_map,
    surfatlas = surfatlas,
    sample_table = sample_table,
    design = design
  )
}

.has_surface_geometry <- function(surfatlas) {
  if (!inherits(surfatlas, "surfatlas")) {
    return(FALSE)
  }

  ok_hemi <- vapply(c("lh_atlas", "rh_atlas"), function(hk) {
    obj <- surfatlas[[hk]]
    if (is.null(obj) || !isS4(obj)) {
      return(FALSE)
    }
    all(c("geometry", "data") %in% methods::slotNames(obj))
  }, logical(1))

  all(ok_hemi)
}

.fallback_brain_plot <- function(surfatlas,
                                 vals = NULL,
                                 palette = "vik",
                                 lim = NULL,
                                 interactive = TRUE,
                                 title = "Parcel Layout (Fallback)") {
  ids <- as.integer(surfatlas$ids)
  if (length(ids) == 0) {
    ids <- seq_len(if (!is.null(vals)) length(vals) else 12L)
  }

  labels <- as.character(surfatlas$labels)
  if (length(labels) != length(ids)) {
    labels <- paste0("Parcel ", ids)
  }

  if (is.null(vals)) {
    value <- rep(0, length(ids))
  } else {
    v <- as.numeric(vals)
    if (length(v) != length(ids)) {
      value <- rep(NA_real_, length(ids))
      names(value) <- ids
      common <- intersect(as.character(ids), names(vals))
      value[common] <- as.numeric(vals[common])
    } else {
      value <- v
    }
  }

  n <- length(ids)
  ncol <- max(1L, ceiling(sqrt(n)))
  nrow <- ceiling(n / ncol)
  grid_idx <- seq_len(n)
  cx <- ((grid_idx - 1L) %% ncol) + 1L
  cy <- nrow - ((grid_idx - 1L) %/% ncol)

  dat <- tibble::tibble(
    parcel_id = ids,
    label = labels,
    value = value,
    x = cx,
    y = cy,
    data_id = as.character(ids),
    tooltip = paste0(labels, "\nValue: ", signif(value, 4))
  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y))
  if (isTRUE(interactive)) {
    p <- p + ggiraph::geom_tile_interactive(
      ggplot2::aes(fill = value, tooltip = tooltip, data_id = data_id),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  } else {
    p <- p + ggplot2::geom_tile(
      ggplot2::aes(fill = value),
      width = 0.94,
      height = 0.94,
      colour = "#ffffff",
      linewidth = 0.4
    )
  }

  if (n <= 30) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = parcel_id),
      size = 2.8,
      colour = "#111827"
    )
  }

  p <- p +
    scico::scale_fill_scico(
      palette = palette,
      limits = lim,
      oob = scales::squish,
      na.value = "#f3f4f6"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, fill = "Value") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "#6b7280", size = 11),
      legend.position = "none"
    )

  if (!isTRUE(interactive)) {
    return(p)
  }

  ggiraph::girafe(ggobj = p)
}

.infer_n_samples <- function(data_source, sample_table = NULL, design = NULL) {
  ds_dim <- dim(data_source)
  if (!is.null(ds_dim) && length(ds_dim) >= 4) {
    return(as.integer(ds_dim[4]))
  }
  if (!is.null(sample_table)) {
    return(nrow(sample_table))
  }
  if (!is.null(design)) {
    return(nrow(design))
  }
  stop(
    "Unable to infer number of samples from data_source. ",
    "Provide a sample_table or design with one row per sample."
  )
}

.validate_cluster_explorer_inputs <- function(data_source,
                                              atlas,
                                              stat_map,
                                              sample_table = NULL,
                                              n_samples,
                                              series_fun = NULL) {
  assertthat::assert_that(
    inherits(atlas, "atlas"),
    msg = "'atlas' must inherit from class 'atlas'."
  )
  assertthat::assert_that(
    methods::is(stat_map, "NeuroVol"),
    msg = "'stat_map' must be a NeuroVol."
  )

  atlas_vol <- .get_atlas_volume(atlas)
  atlas_dims <- dim(atlas_vol)[1:3]
  data_dims <- dim(data_source)
  if (!is.null(data_dims) && length(data_dims) >= 3) {
    data_dims <- data_dims[1:3]
  } else {
    data_dims <- NULL
  }
  stat_dims <- dim(stat_map)[1:3]

  if (!is.null(data_dims)) {
    assertthat::assert_that(
      all(atlas_dims == data_dims),
      msg = paste0(
        "Spatial dimensions of atlas (",
        paste(atlas_dims, collapse = "x"),
        ") and data_source (",
        paste(data_dims, collapse = "x"),
        ") must match when data_source has spatial dims."
      )
    )
  }
  assertthat::assert_that(
    all(atlas_dims == stat_dims),
    msg = paste0(
      "Spatial dimensions of atlas (",
      paste(atlas_dims, collapse = "x"),
      ") and stat_map (",
      paste(stat_dims, collapse = "x"),
      ") must match."
    )
  )

  if (!is.null(sample_table)) {
    assertthat::assert_that(
      is.data.frame(sample_table),
      msg = "'sample_table' must be a data.frame when provided."
    )
    assertthat::assert_that(
      nrow(sample_table) == n_samples,
      msg = paste0(
        "nrow(sample_table) (", nrow(sample_table),
        ") must equal number of samples (", n_samples, ")."
      )
    )
  }

  if (!is.null(series_fun)) {
    assertthat::assert_that(
      is.function(series_fun),
      msg = "'series_fun' must be a function(data_source, i)."
    )
  }

  invisible(TRUE)
}

.normalize_sample_table <- function(sample_table, n_samples) {
  if (is.null(sample_table)) {
    tbl <- tibble::tibble(.sample_index = seq_len(n_samples))
  } else {
    tbl <- tibble::as_tibble(sample_table, .name_repair = "unique")
    tbl$.sample_index <- seq_len(n_samples)
  }
  tbl
}

.normalize_design_table <- function(design, n_samples) {
  if (is.null(design)) {
    return(NULL)
  }

  assertthat::assert_that(
    is.data.frame(design),
    msg = "'design' must be a data.frame when provided."
  )
  assertthat::assert_that(
    nrow(design) == n_samples,
    msg = paste0(
      "nrow(design) (", nrow(design),
      ") must equal number of samples (", n_samples, ")."
    )
  )

  tibble::as_tibble(design, .name_repair = "unique")
}

.merge_sample_and_design <- function(sample_table, design_table) {
  if (is.null(design_table)) {
    return(sample_table)
  }

  # Keep sample index from sample_table as the canonical join row order.
  design_table$.sample_index <- sample_table$.sample_index
  dplyr::left_join(sample_table, design_table, by = ".sample_index",
                   suffix = c("", ".design"))
}

.extract_stat_clusters <- function(stat_map,
                                   threshold,
                                   min_cluster_size,
                                   connectivity,
                                   tail) {
  stat_arr <- as.array(stat_map)
  if (length(dim(stat_arr)) != 3) {
    stop("'stat_map' must be a 3D NeuroVol.")
  }

  out_tbl <- list()
  cluster_voxels <- list()
  cluster_arr <- array(0L, dim = dim(stat_arr))
  global_id <- 0L

  tails <- switch(
    tail,
    positive = "positive",
    negative = "negative",
    two_sided = c("positive", "negative")
  )

  for (sgn in tails) {
    if (sgn == "positive") {
      work_arr <- stat_arr
      mask <- stat_arr > threshold
      prefix <- "P"
    } else {
      work_arr <- -stat_arr
      mask <- stat_arr < -threshold
      prefix <- "N"
    }

    if (!any(mask, na.rm = TRUE)) {
      next
    }

    work_vol <- neuroim2::NeuroVol(work_arr, space = neuroim2::space(stat_map))
    cc <- neuroim2::conn_comp(
      work_vol,
      threshold = threshold,
      cluster_table = TRUE,
      local_maxima = FALSE,
      connect = connectivity
    )

    if (is.null(cc$cluster_table) || nrow(cc$cluster_table) == 0) {
      next
    }

    ct <- cc$cluster_table
    ct <- ct[ct$N >= min_cluster_size, , drop = FALSE]
    if (nrow(ct) == 0) {
      next
    }

    for (i in seq_len(nrow(ct))) {
      local_idx <- as.integer(ct$index[i])
      vox <- cc$voxels[[as.character(local_idx)]]
      if (is.null(vox) || nrow(vox) == 0) {
        next
      }

      vox_mat <- as.matrix(vox[, c("x", "y", "z"), drop = FALSE])
      peak_xyz <- c(as.integer(ct$x[i]),
                    as.integer(ct$y[i]),
                    as.integer(ct$z[i]))
      peak_stat <- stat_arr[matrix(peak_xyz, nrow = 1)]

      global_id <- global_id + 1L
      cluster_id <- paste0(prefix, global_id)

      cluster_voxels[[cluster_id]] <- vox_mat
      idx_lin <- .grid_to_linear_index(vox_mat, dim(cluster_arr))
      cluster_arr[idx_lin] <- global_id

      out_tbl[[length(out_tbl) + 1L]] <- tibble::tibble(
        cluster_id = cluster_id,
        sign = sgn,
        component_id = local_idx,
        n_voxels = as.integer(ct$N[i]),
        peak_x = peak_xyz[1],
        peak_y = peak_xyz[2],
        peak_z = peak_xyz[3],
        max_stat = as.numeric(peak_stat),
        peak_coord = paste0("(", peak_xyz[1], ", ", peak_xyz[2], ", ",
                            peak_xyz[3], ")")
      )
    }
  }

  cluster_tbl <- if (length(out_tbl) == 0) {
    tibble::tibble(
      cluster_id = character(0),
      sign = character(0),
      component_id = integer(0),
      n_voxels = integer(0),
      peak_x = integer(0),
      peak_y = integer(0),
      peak_z = integer(0),
      max_stat = numeric(0),
      peak_coord = character(0)
    )
  } else {
    dplyr::bind_rows(out_tbl)
  }

  list(
    cluster_table = cluster_tbl,
    cluster_voxels = cluster_voxels,
    cluster_index = neuroim2::NeuroVol(cluster_arr, space = neuroim2::space(stat_map))
  )
}

.annotate_clusters_with_atlas <- function(cluster_table,
                                          cluster_voxels,
                                          atlas,
                                          stat_map) {
  if (nrow(cluster_table) == 0 || length(cluster_voxels) == 0) {
    cluster_table$atlas_label_primary <- character(0)
    cluster_table$n_parcels <- integer(0)
    cluster_table$parcel_overlap <- numeric(0)
    return(list(
      cluster_table = cluster_table,
      cluster_parcels = tibble::tibble(
        cluster_id = character(0),
        sign = character(0),
        parcel_id = integer(0),
        parcel_label = character(0),
        n_voxels = integer(0),
        frac = numeric(0),
        peak_stat = numeric(0),
        max_pos = numeric(0),
        min_neg = numeric(0)
      )
    ))
  }

  atlas_arr <- .atlas_volume_array(.get_atlas_volume(atlas))
  stat_arr <- as.array(stat_map)
  meta <- roi_metadata(atlas)
  label_map <- stats::setNames(as.character(meta$label), as.character(meta$id))

  tbl <- cluster_table
  tbl$atlas_label_primary <- NA_character_
  tbl$n_parcels <- 0L
  tbl$parcel_overlap <- 0

  parcel_rows <- list()

  for (i in seq_len(nrow(tbl))) {
    cid <- tbl$cluster_id[i]
    vox <- cluster_voxels[[cid]]
    if (is.null(vox) || nrow(vox) == 0) {
      next
    }

    idx_lin <- .grid_to_linear_index(vox, dim(atlas_arr))
    parcel_ids <- atlas_arr[idx_lin]
    stat_vals <- stat_arr[idx_lin]
    keep <- !is.na(parcel_ids) & parcel_ids != 0
    parcel_ids <- as.integer(parcel_ids[keep])
    stat_vals <- as.numeric(stat_vals[keep])

    if (length(parcel_ids) == 0) {
      next
    }

    tab <- sort(table(parcel_ids), decreasing = TRUE)
    pids <- as.integer(names(tab))
    pcounts <- as.integer(tab)
    pfrac <- pcounts / sum(pcounts)

    tbl$atlas_label_primary[i] <- label_map[as.character(pids[1])]
    tbl$n_parcels[i] <- length(pids)
    tbl$parcel_overlap[i] <- pfrac[1]

    for (j in seq_along(pids)) {
      pid <- pids[j]
      svals <- stat_vals[parcel_ids == pid]
      svals <- as.numeric(svals)
      finite <- is.finite(svals)
      svals_finite <- svals[finite]

      peak_stat <- if (length(svals_finite) == 0) {
        NA_real_
      } else {
        svals_finite[which.max(abs(svals_finite))]
      }
      max_pos <- if (any(svals_finite > 0, na.rm = TRUE)) {
        max(svals_finite[svals_finite > 0], na.rm = TRUE)
      } else {
        NA_real_
      }
      min_neg <- if (any(svals_finite < 0, na.rm = TRUE)) {
        min(svals_finite[svals_finite < 0], na.rm = TRUE)
      } else {
        NA_real_
      }

      parcel_rows[[length(parcel_rows) + 1L]] <- tibble::tibble(
        cluster_id = cid,
        sign = tbl$sign[i],
        parcel_id = pid,
        parcel_label = label_map[as.character(pid)],
        n_voxels = pcounts[j],
        frac = pfrac[j],
        peak_stat = peak_stat,
        max_pos = max_pos,
        min_neg = min_neg
      )
    }
  }

  cluster_parcels <- if (length(parcel_rows) == 0) {
    tibble::tibble(
      cluster_id = character(0),
      sign = character(0),
      parcel_id = integer(0),
      parcel_label = character(0),
      n_voxels = integer(0),
      frac = numeric(0),
      peak_stat = numeric(0),
      max_pos = numeric(0),
      min_neg = numeric(0)
    )
  } else {
    dplyr::bind_rows(parcel_rows)
  }

  list(cluster_table = tbl, cluster_parcels = cluster_parcels)
}

.compute_cluster_timeseries <- function(data_source,
                                        cluster_voxels,
                                        sample_table,
                                        series_fun = NULL,
                                        signal_fun = mean,
                                        signal_fun_args = list(na.rm = TRUE),
                                        series_cache_env = NULL) {
  if (length(cluster_voxels) == 0) {
    return(tibble::tibble())
  }

  stopifnot(is.function(signal_fun))
  n_samples <- nrow(sample_table)
  ids <- names(cluster_voxels)
  out <- vector("list", length(ids))
  names(out) <- ids

  for (k in seq_along(ids)) {
    cid <- ids[k]
    vox <- cluster_voxels[[cid]]
    series_mat <- .extract_series_matrix(
      data_source = data_source,
      voxel_coords = vox,
      n_samples = n_samples,
      series_fun = series_fun,
      cache_env = series_cache_env,
      cache_key = cid
    )

    sig <- vapply(seq_len(n_samples), function(t) {
      vals <- series_mat[t, , drop = TRUE]
      do.call(signal_fun, c(list(vals), signal_fun_args))
    }, numeric(1))

    out[[k]] <- tibble::tibble(
      .sample_index = seq_len(n_samples),
      cluster_id = cid,
      signal = sig
    )
  }

  ret <- dplyr::bind_rows(out)
  dplyr::left_join(ret, sample_table, by = ".sample_index")
}

.extract_series_matrix <- function(data_source,
                                   voxel_coords,
                                   n_samples,
                                   series_fun = NULL,
                                   cache_env = NULL,
                                   cache_key = NULL) {
  if (!is.null(cache_env) && !is.null(cache_key) &&
      exists(cache_key, envir = cache_env, inherits = FALSE)) {
    cached <- get(cache_key, envir = cache_env, inherits = FALSE)
    return(.coerce_series_matrix(cached, n_samples = n_samples))
  }

  raw <- tryCatch(
    {
      if (!is.null(series_fun)) {
        series_fun(data_source, voxel_coords)
      } else {
        neuroim2::series(data_source, voxel_coords)
      }
    },
    error = function(e) {
      stop(
        "Failed to extract sample series from data_source. ",
        "Ensure neuroim2::series(data_source, voxel_coords) works, ",
        "or supply a compatible series_fun(data_source, voxel_coords). ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  out <- .coerce_series_matrix(raw, n_samples = n_samples)
  if (!is.null(cache_env) && !is.null(cache_key)) {
    assign(cache_key, out, envir = cache_env)
  }
  out
}

.coerce_series_matrix <- function(x, n_samples) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (is.null(dim(x))) {
    x <- matrix(as.numeric(x), ncol = 1)
  } else {
    x <- as.matrix(x)
  }

  if (nrow(x) == n_samples) {
    return(x)
  }
  if (ncol(x) == n_samples) {
    return(t(x))
  }
  if (length(x) == n_samples) {
    return(matrix(as.numeric(x), ncol = 1))
  }

  stop(
    "Extracted series has incompatible shape. Expected ", n_samples,
    " sample rows, got matrix ", nrow(x), "x", ncol(x), ".",
    call. = FALSE
  )
}

.resolve_cluster_ts <- function(dat,
                                selected_cluster_ids,
                                data_source,
                                series_fun,
                                signal_fun,
                                signal_fun_args,
                                series_cache_env,
                                ts_cache_env) {
  ids <- unique(as.character(selected_cluster_ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return(tibble::tibble())

  cached_ids <- ids[vapply(ids, function(cid) {
    exists(cid, envir = ts_cache_env, inherits = FALSE)
  }, logical(1))]
  missing_ids <- setdiff(ids, cached_ids)

  if (length(missing_ids) > 0) {
    missing_vox <- dat$cluster_voxels[missing_ids]
    missing_vox <- missing_vox[!vapply(missing_vox, is.null, logical(1))]
    if (length(missing_vox) > 0) {
      ts_new <- .compute_cluster_timeseries(
        data_source = data_source,
        cluster_voxels = missing_vox,
        sample_table = dat$sample_table,
        series_fun = series_fun,
        signal_fun = signal_fun,
        signal_fun_args = signal_fun_args,
        series_cache_env = series_cache_env
      )
      if (nrow(ts_new) > 0) {
        if (nrow(dat$cluster_table) > 0) {
          ts_new <- dplyr::left_join(
            ts_new,
            dat$cluster_table[, c("cluster_id", "sign"), drop = FALSE],
            by = "cluster_id"
          )
        }
        for (cid in unique(ts_new$cluster_id)) {
          assign(cid,
                 ts_new[ts_new$cluster_id == cid, , drop = FALSE],
                 envir = ts_cache_env)
        }
      }
    }
  }

  out <- lapply(ids, function(cid) {
    if (exists(cid, envir = ts_cache_env, inherits = FALSE)) {
      get(cid, envir = ts_cache_env, inherits = FALSE)
    } else {
      NULL
    }
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) return(tibble::tibble())
  dplyr::bind_rows(out)
}

.build_cluster_overlay_volume <- function(stat_map,
                                          cluster_voxels,
                                          selected_cluster_ids = NULL) {
  stat_arr <- as.array(stat_map)
  out_arr <- array(0, dim = dim(stat_arr))

  ids <- names(cluster_voxels)
  if (!is.null(selected_cluster_ids) && length(selected_cluster_ids) > 0) {
    ids <- intersect(ids, as.character(selected_cluster_ids))
  }

  if (length(ids) == 0) {
    return(neuroim2::NeuroVol(out_arr, space = neuroim2::space(stat_map)))
  }

  for (cid in ids) {
    vox <- cluster_voxels[[cid]]
    if (is.null(vox) || nrow(vox) == 0) next
    idx <- .grid_to_linear_index(vox, dim(out_arr))
    out_arr[idx] <- stat_arr[idx]
  }

  neuroim2::NeuroVol(out_arr, space = neuroim2::space(stat_map))
}

.project_cluster_overlay <- function(cluster_vol,
                                     surfatlas,
                                     space_override = NULL,
                                     density_override = NULL,
                                     resolution_override = NULL,
                                     fun = c("avg", "nn", "mode"),
                                     sampling = c("midpoint",
                                                  "normal_line",
                                                  "thickness")) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)

  out <- list(lh = NULL, rh = NULL)
  meta <- list(surface_space = NULL, hemis = list())

  for (hemi in c("lh", "rh")) {
    atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
    if (is.null(atlas_hemi)) next

    pair <- .resolve_overlay_surface_pair(
      surfatlas = surfatlas,
      hemi = hemi,
      space_override = space_override,
      density_override = density_override,
      resolution_override = resolution_override
    )
    meta$surface_space <- pair$surface_space

    target_n <- length(atlas_hemi@data)
    vals <- .project_overlay_one_hemi(
      cluster_vol = cluster_vol,
      surf_wm = pair$white,
      surf_pial = pair$pial,
      target_n = target_n,
      fun = fun,
      sampling = sampling
    )

    out[[hemi]] <- vals
    meta$hemis[[hemi]] <- list(
      target_vertices = target_n,
      projected_vertices = length(vals),
      finite_vertices = sum(is.finite(vals))
    )
  }

  list(overlay = out, meta = meta)
}

.overlay_projection_diagnostics <- function(cluster_vol,
                                            projection,
                                            threshold,
                                            sampling,
                                            fun) {
  vals <- projection$overlay
  meta <- projection$meta
  cluster_vals <- as.array(cluster_vol)
  nonzero <- sum(cluster_vals != 0, na.rm = TRUE)

  hemi_stats <- lapply(c("lh", "rh"), function(h) {
    x <- vals[[h]]
    if (is.null(x)) {
      data.frame(
        hemi = h,
        target_vertices = NA_integer_,
        finite_vertices = 0L,
        above_threshold = 0L,
        finite_min = NA_real_,
        finite_max = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      finite <- is.finite(x)
      data.frame(
        hemi = h,
        target_vertices = if (!is.null(meta$hemis[[h]]$target_vertices)) {
          as.integer(meta$hemis[[h]]$target_vertices)
        } else {
          as.integer(length(x))
        },
        finite_vertices = as.integer(sum(finite)),
        above_threshold = as.integer(sum(finite & abs(x) >= threshold)),
        finite_min = if (any(finite)) min(x[finite]) else NA_real_,
        finite_max = if (any(finite)) max(x[finite]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  })
  hemi_tbl <- do.call(rbind, hemi_stats)

  list(
    cluster_voxels_nonzero = nonzero,
    surface_space = meta$surface_space,
    projection_fun = fun,
    projection_sampling = sampling,
    overlay_threshold = threshold,
    hemi = hemi_tbl
  )
}

.project_overlay_one_hemi <- function(cluster_vol,
                                      surf_wm,
                                      surf_pial,
                                      target_n,
                                      fun,
                                      sampling) {
  proj <- tryCatch(
    neurosurf::vol_to_surf(
      surf_wm = surf_wm,
      surf_pial = surf_pial,
      vol = cluster_vol,
      fun = fun,
      sampling = sampling,
      fill = 0
    ),
    error = function(e) NULL
  )

  vals <- .surface_values_to_numeric(proj)
  if (is.null(vals)) {
    return(rep(NA_real_, target_n))
  }

  if (length(vals) != target_n) {
    return(rep(NA_real_, target_n))
  }

  vals
}

.surface_values_to_numeric <- function(x) {
  if (is.null(x)) return(NULL)

  vals <- tryCatch(neurosurf::values(x), error = function(e) NULL)
  if (is.null(vals)) {
    vals <- tryCatch(x@data, error = function(e) NULL)
  }
  if (is.null(vals)) return(NULL)
  as.numeric(vals)
}

.resolve_overlay_surface_pair <- function(surfatlas,
                                          hemi = c("lh", "rh"),
                                          space_override = NULL,
                                          density_override = NULL,
                                          resolution_override = NULL) {
  hemi <- match.arg(hemi)
  atlas_hemi <- surfatlas[[paste0(hemi, "_atlas")]]
  current_geom <- atlas_hemi@geometry
  surf_type <- if (!is.null(surfatlas$surf_type)) surfatlas$surf_type else NA_character_

  white <- if (identical(surf_type, "white")) current_geom else NULL
  pial <- if (identical(surf_type, "pial")) current_geom else NULL

  surface_space <- if (!is.null(space_override)) {
    space_override
  } else if (!is.null(surfatlas$surface_space)) {
    surfatlas$surface_space
  } else {
    "fsaverage6"
  }

  if (is.null(white)) {
    white <- .load_overlay_surface_geometry(
      surface_space = surface_space,
      surface_type = "white",
      hemi = hemi,
      density_override = density_override,
      resolution_override = resolution_override
    )
  }
  if (is.null(pial)) {
    pial <- .load_overlay_surface_geometry(
      surface_space = surface_space,
      surface_type = "pial",
      hemi = hemi,
      density_override = density_override,
      resolution_override = resolution_override
    )
  }

  if (is.null(white)) white <- current_geom
  if (is.null(pial)) pial <- current_geom

  list(white = white, pial = pial, surface_space = surface_space)
}

.surface_template_defaults <- function(surface_space) {
  if (is.null(surface_space) || !nzchar(surface_space)) {
    return(list(template_id = "fsaverage", density = "41k", resolution = "06"))
  }

  switch(
    as.character(surface_space),
    fsaverage6 = list(template_id = "fsaverage", density = "41k",
                      resolution = "06"),
    fsaverage5 = list(template_id = "fsaverage", density = "10k",
                      resolution = "05"),
    fsaverage = list(template_id = "fsaverage", density = "164k",
                     resolution = NULL),
    list(template_id = surface_space, density = NULL, resolution = NULL)
  )
}

.load_overlay_surface_geometry <- function(surface_space,
                                           surface_type = c("white", "pial"),
                                           hemi = c("lh", "rh"),
                                           density_override = NULL,
                                           resolution_override = NULL) {
  surface_type <- match.arg(surface_type)
  hemi <- match.arg(hemi)

  # Fast packaged fallback for fsaverage6 surfaces.
  if (identical(surface_space, "fsaverage6") &&
      is.null(density_override) &&
      is.null(resolution_override)) {
    fsaverage <- NULL
    utils::data("fsaverage", envir = environment())
    if (exists("fsaverage", envir = environment(), inherits = FALSE)) {
      fsaverage <- get("fsaverage", envir = environment())
      geom_name <- paste0(hemi, "_", surface_type)
      if (!is.null(fsaverage[[geom_name]])) {
        return(fsaverage[[geom_name]])
      }
    }
  }

  defaults <- .surface_template_defaults(surface_space)
  density <- if (!is.null(density_override)) density_override else defaults$density
  resolution <- if (!is.null(resolution_override)) {
    resolution_override
  } else {
    defaults$resolution
  }

  hemi_tf <- if (identical(hemi, "lh")) "L" else "R"
  tryCatch(
    load_surface_template(
      template_id = defaults$template_id,
      surface_type = surface_type,
      hemi = hemi_tf,
      density = density,
      resolution = resolution
    ),
    error = function(e) NULL
  )
}

.atlas_volume_array <- function(vol) {
  if (methods::is(vol, "ClusteredNeuroVol")) {
    arr <- array(0L, dim = dim(vol))
    arr[which(vol@mask)] <- as.integer(vol@clusters)
    return(arr)
  }
  if (methods::is(vol, "NeuroVol")) {
    arr <- as.array(vol)
    dim(arr) <- dim(vol)[1:3]
    storage.mode(arr) <- "integer"
    return(arr)
  }
  stop("Unsupported volume class for atlas conversion.")
}

.grid_to_linear_index <- function(grid_xyz, dims3) {
  stopifnot(ncol(grid_xyz) == 3)
  x <- as.integer(grid_xyz[, 1])
  y <- as.integer(grid_xyz[, 2])
  z <- as.integer(grid_xyz[, 3])
  as.integer(x + (y - 1L) * dims3[1] + (z - 1L) * dims3[1] * dims3[2])
}

.clusters_for_parcels <- function(cluster_parcels, parcel_ids) {
  if (nrow(cluster_parcels) == 0 || length(parcel_ids) == 0) {
    return(character(0))
  }

  keep <- cluster_parcels$parcel_id %in% as.integer(parcel_ids)
  rows <- cluster_parcels[keep, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(character(0))
  }

  score <- stats::aggregate(
    rows$frac,
    by = list(cluster_id = rows$cluster_id),
    FUN = sum
  )
  score <- score[order(score$x, decreasing = TRUE), , drop = FALSE]
  as.character(score$cluster_id)
}

.parse_plot_brain_selection_ids <- function(ids) {
  ids <- as.character(ids)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) {
    return(tibble::tibble(
      raw_id = character(0),
      panel = character(0),
      parcel_id = integer(0),
      shape_id = integer(0)
    ))
  }

  rows <- lapply(ids, function(id) {
    parts <- strsplit(id, "::", fixed = TRUE)[[1]]
    if (length(parts) == 3) {
      pid <- suppressWarnings(as.integer(parts[2]))
      sid <- suppressWarnings(as.integer(parts[3]))
      if (is.finite(pid) && is.finite(sid)) {
        return(tibble::tibble(
          raw_id = id,
          panel = as.character(parts[1]),
          parcel_id = as.integer(pid),
          shape_id = as.integer(sid)
        ))
      }
    }

    pid <- suppressWarnings(as.integer(id))
    tibble::tibble(
      raw_id = id,
      panel = NA_character_,
      parcel_id = if (is.finite(pid)) as.integer(pid) else NA_integer_,
      shape_id = NA_integer_
    )
  })

  dplyr::bind_rows(rows)
}

.surface_pick_surface_to_world <- function(geometry, surface_xyz) {
  xform <- tryCatch(neurosurf::surf_to_world(geometry), error = function(e) NULL)
  if (is.null(xform) || !is.matrix(xform) || !all(dim(xform) == c(4, 4))) {
    return(as.numeric(surface_xyz))
  }

  v <- matrix(as.numeric(surface_xyz), nrow = 1)
  rot <- xform[1:3, 1:3, drop = FALSE]
  trans <- matrix(xform[1:3, 4], nrow = 1)
  as.numeric(v %*% t(rot) + trans)
}

.surface_pick_round_clip_grid <- function(grid_xyz, dims3) {
  if (is.null(dim(grid_xyz))) {
    g <- as.integer(round(as.numeric(grid_xyz)))
  } else {
    g <- as.integer(round(as.numeric(grid_xyz[1, ])))
  }
  d <- as.integer(dims3)
  pmax(1L, pmin(d, g))
}

.surface_pick_lookup_from_polygons <- function(poly, panel_ctx, stat_map) {
  empty <- tibble::tibble(
    data_id = character(0),
    panel = character(0),
    parcel_id = integer(0),
    shape_id = integer(0),
    vertex_index = integer(0),
    surface_x = numeric(0),
    surface_y = numeric(0),
    surface_z = numeric(0),
    world_x = numeric(0),
    world_y = numeric(0),
    world_z = numeric(0),
    grid_x = integer(0),
    grid_y = integer(0),
    grid_z = integer(0)
  )

  if (is.null(poly) || nrow(poly) == 0) {
    return(empty)
  }

  key <- unique(poly[, c("panel", "parcel_id", "poly_id"), drop = FALSE])
  rows <- vector("list", nrow(key))
  dims3 <- dim(stat_map)[1:3]
  vol_space <- neuroim2::space(stat_map)

  for (i in seq_len(nrow(key))) {
    panel <- as.character(key$panel[i])
    parcel_id <- as.integer(key$parcel_id[i])
    shape_id <- as.integer(key$poly_id[i])
    ctx <- panel_ctx[[panel]]
    if (is.null(ctx)) next

    sub <- poly[
      poly$panel == panel &
        poly$parcel_id == parcel_id &
        poly$poly_id == shape_id,
      ,
      drop = FALSE
    ]
    if (nrow(sub) == 0) next

    centroid <- c(mean(sub$x), mean(sub$y))
    cand <- which(ctx$parcels == parcel_id)
    if (length(cand) == 0) cand <- seq_len(nrow(ctx$xy))
    if (length(cand) == 0) next

    d2 <- (ctx$xy[cand, 1] - centroid[1])^2 +
      (ctx$xy[cand, 2] - centroid[2])^2
    best <- cand[which.min(d2)]
    surface_xyz <- as.numeric(ctx$verts[best, ])
    world_xyz <- .surface_pick_surface_to_world(ctx$geometry, surface_xyz)
    grid_xyz <- neuroim2::coord_to_grid(vol_space, matrix(world_xyz, nrow = 1))
    grid_xyz <- .surface_pick_round_clip_grid(grid_xyz, dims3 = dims3)

    rows[[i]] <- tibble::tibble(
      data_id = .encode_plot_brain_data_id(
        panel = panel,
        parcel_id = parcel_id,
        shape_id = shape_id
      ),
      panel = panel,
      parcel_id = parcel_id,
      shape_id = shape_id,
      vertex_index = as.integer(best),
      surface_x = surface_xyz[1],
      surface_y = surface_xyz[2],
      surface_z = surface_xyz[3],
      world_x = world_xyz[1],
      world_y = world_xyz[2],
      world_z = world_xyz[3],
      grid_x = grid_xyz[1],
      grid_y = grid_xyz[2],
      grid_z = grid_xyz[3]
    )
  }

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(empty)
  }
  dplyr::bind_rows(rows)
}

.build_plot_brain_surface_pick_lookup <- function(surfatlas,
                                                  stat_map,
                                                  views = c("lateral", "medial"),
                                                  hemis = c("left", "right"),
                                                  surface = "inflated") {
  empty <- .surface_pick_lookup_from_polygons(
    poly = NULL,
    panel_ctx = list(),
    stat_map = stat_map
  )

  if (!.has_surface_geometry(surfatlas)) {
    return(empty)
  }

  build <- .build_merged_polygon_data_memo(
    surfatlas = surfatlas,
    views = views,
    surface = surface
  )
  poly <- build$polygons
  if (is.null(poly) || nrow(poly) == 0) {
    return(empty)
  }

  poly <- poly[poly$hemi %in% hemis, , drop = FALSE]
  if (nrow(poly) == 0) {
    return(empty)
  }

  panel_ctx <- list()
  hemi_key <- c(left = "lh", right = "rh")
  for (h in hemis) {
    hk <- hemi_key[[h]]
    atlas_hemi <- surfatlas[[paste0(hk, "_atlas")]]
    if (is.null(atlas_hemi)) next

    geom <- atlas_hemi@geometry
    verts <- t(geom@mesh$vb[1:3, , drop = FALSE])
    parcels <- as.integer(atlas_hemi@data)
    if (length(parcels) != nrow(verts)) {
      parcels <- rep(NA_integer_, nrow(verts))
    }

    for (v in views) {
      panel <- paste0(tools::toTitleCase(h), " ", tools::toTitleCase(v))
      proj <- .project_view(verts = verts, view = v, hemi = h)
      panel_ctx[[panel]] <- list(
        xy = proj$xy,
        verts = verts,
        parcels = parcels,
        geometry = geom
      )
    }
  }

  .surface_pick_lookup_from_polygons(
    poly = poly,
    panel_ctx = panel_ctx,
    stat_map = stat_map
  )
}

.clusters_for_grid_centers <- function(cluster_voxels,
                                       centers,
                                       radius = 0,
                                       fallback_nearest = TRUE) {
  if (length(cluster_voxels) == 0) return(character(0))

  centers <- as.matrix(centers)
  if (is.null(dim(centers)) || ncol(centers) != 3 || nrow(centers) == 0) {
    return(character(0))
  }

  rad <- suppressWarnings(as.numeric(radius))
  if (!is.finite(rad) || rad < 0) rad <- 0
  rad2 <- rad^2

  ids <- names(cluster_voxels)
  if (is.null(ids)) ids <- paste0("K", seq_along(cluster_voxels))
  score <- rep(Inf, length(cluster_voxels))
  names(score) <- ids

  for (i in seq_along(cluster_voxels)) {
    vox <- as.matrix(cluster_voxels[[i]])
    if (is.null(vox) || nrow(vox) == 0 || ncol(vox) != 3) next

    dmin <- Inf
    for (j in seq_len(nrow(centers))) {
      ctr <- matrix(centers[j, ], nrow = nrow(vox), ncol = 3, byrow = TRUE)
      d2 <- rowSums((vox - ctr)^2)
      dmin <- min(dmin, min(d2))
    }
    score[i] <- dmin
  }

  inside <- names(score)[is.finite(score) & score <= rad2]
  if (length(inside) > 0) {
    return(inside[order(score[inside], decreasing = FALSE)])
  }

  if (isTRUE(fallback_nearest)) {
    finite <- which(is.finite(score))
    if (length(finite) == 0) return(character(0))
    best <- finite[which.min(score[finite])]
    return(names(score)[best])
  }

  character(0)
}

.parcel_values_from_clusters <- function(cluster_parcels,
                                         atlas_ids,
                                         selected_cluster_ids = NULL,
                                         mode = c("dominant",
                                                  "positive_only",
                                                  "negative_only")) {
  mode <- match.arg(mode)
  vals <- rep(NA_real_, length(atlas_ids))
  names(vals) <- as.character(atlas_ids)

  if (nrow(cluster_parcels) == 0) {
    return(vals)
  }

  cp <- cluster_parcels
  if (!is.null(selected_cluster_ids) && length(selected_cluster_ids) > 0) {
    cp <- cp[cp$cluster_id %in% selected_cluster_ids, , drop = FALSE]
  }
  if (nrow(cp) == 0) {
    return(vals)
  }

  parcel_groups <- split(cp, cp$parcel_id)
  out_vals <- vapply(parcel_groups, function(df) {
    if (mode == "dominant") {
      peak <- df$peak_stat[which.max(abs(df$peak_stat))]
      as.numeric(peak)
    } else if (mode == "positive_only") {
      x <- df$max_pos
      x <- x[is.finite(x)]
      if (length(x) == 0) NA_real_ else max(x)
    } else {
      x <- df$min_neg
      x <- x[is.finite(x)]
      if (length(x) == 0) NA_real_ else min(x)
    }
  }, numeric(1))

  common <- intersect(names(out_vals), names(vals))
  vals[common] <- out_vals[common]
  vals
}

.build_design_plot <- function(data, x_var, collapse_vars = NULL) {
  collapse_vars <- collapse_vars[collapse_vars %in% names(data)]
  collapse_vars <- setdiff(collapse_vars, c("cluster_id", "signal"))

  plot_data <- data
  if (length(collapse_vars) > 0) {
    group_vars <- unique(c("cluster_id", x_var, collapse_vars))
    plot_data <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(signal = mean(signal, na.rm = TRUE), .groups = "drop")
  }

  x <- plot_data[[x_var]]
  xtype <- infer_design_var_type(x)

  if (xtype == "continuous") {
    if (identical(x_var, ".sample_index") ||
        inherits(x, "Date") || inherits(x, "POSIXt")) {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = cluster_id)
      ) +
        ggplot2::geom_line(alpha = 0.7) +
        ggplot2::geom_point(size = 1.5, alpha = 0.8)
    } else {
      p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[x_var]],
                     y = signal,
                     color = cluster_id)
      ) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::geom_smooth(se = FALSE, method = "loess")
    }
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = factor(.data[[x_var]]),
                   y = signal,
                   color = cluster_id)
    ) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.25) +
      ggplot2::geom_jitter(width = 0.15, alpha = 0.75, size = 1.5)
  }

  if (length(unique(plot_data$cluster_id)) > 1) {
    p <- p + ggplot2::facet_wrap(~ cluster_id, scales = "free_y")
  }

  p +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#f3f4f6", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(color = "#e5e7eb", linewidth = 0.4),
      axis.title = ggplot2::element_text(color = "#374151"),
      axis.text = ggplot2::element_text(color = "#111827"),
      strip.background = ggplot2::element_rect(fill = "#f9fafb", color = "#e5e7eb"),
      strip.text = ggplot2::element_text(color = "#111827", face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = "#374151"),
      legend.text = ggplot2::element_text(color = "#111827")
    ) +
    ggplot2::labs(
      x = x_var,
      y = "Cluster Signal",
      color = "Cluster"
    )
}

.empty_plot <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

.cluster_explorer_css <- function() {
  "
:root {
  --ce-bg: #f6f7f8;
  --ce-bg-soft: #fbfbfb;
  --ce-card: #ffffff;
  --ce-border: #e6e8eb;
  --ce-text: #121417;
  --ce-muted: #6a6f78;
  --ce-accent: #111111;
  --ce-ring: rgba(17, 17, 17, 0.12);
  --ce-surface: #f9fafb;
  --ce-surface-2: #f1f3f5;
}

html, body {
  background:
    radial-gradient(1200px 700px at -8% -18%, #ffffff 0%, rgba(255,255,255,0) 58%),
    radial-gradient(900px 500px at 108% 2%, #f0f4f7 0%, rgba(240,244,247,0) 54%),
    var(--ce-bg);
  color: var(--ce-text);
  font-family: \"Geist\", \"IBM Plex Sans\", \"Avenir Next\", \"Segoe UI\", sans-serif;
  text-rendering: geometricPrecision;
}

body {
  min-height: 100vh;
}

.ce-shell {
  max-width: 1520px;
  margin: 0 auto;
  padding: 18px 14px 22px;
}

.ce-topbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 14px;
  margin-bottom: 12px;
  padding: 10px 12px;
  background: rgba(255, 255, 255, 0.72);
  border: 1px solid rgba(230, 232, 235, 0.9);
  border-radius: 14px;
  backdrop-filter: blur(6px);
}

.ce-title-wrap h1 {
  margin: 0;
  font-size: 22px;
  line-height: 1.2;
  letter-spacing: -0.03em;
  font-weight: 620;
}

.ce-eyebrow {
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.14em;
  color: var(--ce-muted);
  margin-bottom: 4px;
}

.ce-subtitle {
  margin: 6px 0 0;
  color: var(--ce-muted);
  font-size: 12.5px;
  line-height: 1.45;
  max-width: 760px;
}

.ce-actions {
  display: flex;
  align-items: center;
  gap: 8px;
}

.ce-card {
  background: var(--ce-card);
  border: 1px solid var(--ce-border);
  border-radius: 14px;
  box-shadow:
    0 1px 2px rgba(18, 20, 23, 0.035),
    0 10px 28px rgba(18, 20, 23, 0.035);
  animation: ce-rise 220ms ease-out both;
}

@keyframes ce-rise {
  from {
    opacity: 0;
    transform: translateY(5px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.ce-controls {
  padding: 14px 14px 8px;
  position: sticky;
  top: 8px;
}

.ce-group {
  border: 1px solid var(--ce-surface-2);
  border-radius: 10px;
  background: #fbfcfd;
  margin-bottom: 8px;
  overflow: visible;
  position: relative;
}

.ce-group summary {
  cursor: pointer;
  list-style: none;
  font-size: 10.5px;
  text-transform: uppercase;
  letter-spacing: 0.11em;
  color: var(--ce-muted);
  font-weight: 620;
  padding: 8px 10px;
  user-select: none;
}

.ce-group summary::-webkit-details-marker {
  display: none;
}

.ce-group summary::after {
  content: \"+\";
  float: right;
  color: #9ca3af;
  font-size: 12px;
}

.ce-group[open] summary::after {
  content: \"-\";
}

.ce-group-body {
  padding: 4px 9px 8px;
  border-top: 1px solid var(--ce-surface-2);
}

.ce-controls .form-group {
  margin-bottom: 7px;
}

.ce-controls label {
  font-size: 11px;
  color: #2f3640;
  font-weight: 520;
  letter-spacing: 0.01em;
}

.ce-help-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 14px;
  height: 14px;
  margin-left: 6px;
  border-radius: 50%;
  border: 1px solid #cfd6dd;
  color: #6b7280;
  font-size: 10px;
  font-weight: 700;
  line-height: 1;
  cursor: help;
  vertical-align: middle;
  background: #ffffff;
}

.ce-help-icon:hover {
  color: #374151;
  border-color: #9fa8b2;
}

.ce-panel {
  padding: 8px 10px 9px;
  margin-bottom: 11px;
}

.ce-panel-head {
  font-size: 10.5px;
  text-transform: uppercase;
  letter-spacing: 0.12em;
  color: var(--ce-muted);
  margin: 2px 4px 7px;
}

.ce-panel-head-with-action {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
}

#analysis_open_btn,
#analysis_close_btn {
  margin: 0 !important;
}

.ce-help {
  font-size: 11px;
  line-height: 1.35;
  color: var(--ce-muted);
  margin-top: 5px;
}

.ce-prefetch-status {
  font-size: 11px;
  line-height: 1.35;
  color: var(--ce-muted);
  margin-top: 3px;
}

.ce-btn,
.ce-btn.btn,
.ce-controls .btn {
  border-radius: 10px !important;
  font-weight: 560;
  font-size: 11.5px;
  letter-spacing: 0.01em;
  padding: 7px 12px;
  transition: all 140ms ease;
}

.ce-btn-primary,
#apply_btn {
  background: var(--ce-accent) !important;
  color: #f9fafb !important;
  border: 1px solid var(--ce-accent) !important;
  box-shadow: 0 1px 0 rgba(255, 255, 255, 0.12) inset;
}

.ce-btn-ghost,
#reset_filters_btn,
#replot_table_btn,
#analysis_open_btn,
#analysis_close_btn,
#download_clusters_csv,
#download_signal_png,
#download_brain_png {
  background: linear-gradient(180deg, #ffffff 0%, #fcfcfd 100%) !important;
  color: #1f2933 !important;
  border: 1px solid var(--ce-border) !important;
}

.ce-btn:hover,
.ce-controls .btn:hover {
  transform: translateY(-1px);
}

.ce-btn-xs {
  padding: 5px 10px !important;
  font-size: 11px !important;
  min-height: 30px;
}

.ce-btn-block,
#download_clusters_csv,
#download_signal_png,
#download_brain_png,
#replot_table_btn {
  width: 100%;
  margin-bottom: 6px;
}

.ce-controls .form-control,
.ce-controls .selectize-input,
.ce-controls .selectize-dropdown,
.ce-controls .js-range-slider {
  border-radius: 9px !important;
  border-color: #d6dbe1 !important;
  background: #ffffff;
  box-shadow: none !important;
}

.ce-controls .selectize-dropdown {
  z-index: 2600 !important;
}

.ce-controls .form-control:focus,
.ce-controls .selectize-input.focus {
  border-color: #9fa8b2 !important;
  box-shadow: 0 0 0 3px var(--ce-ring) !important;
}

.ce-controls .form-control {
  height: 32px;
  font-size: 11.5px;
}

.ce-controls .checkbox {
  margin-top: 2px;
}

.ce-controls .irs--shiny .irs-bar,
.ce-controls .irs--shiny .irs-single {
  background: #111111;
  border-color: #111111;
}

.ce-controls .irs--shiny .irs-from,
.ce-controls .irs--shiny .irs-to {
  background: #111111;
}

.ce-plot-controls {
  display: flex;
  align-items: flex-end;
  gap: 8px;
  flex-wrap: wrap;
  padding: 4px 2px 8px;
}

.ce-plot-field {
  flex: 1 1 180px;
  min-width: 170px;
}

.ce-plot-field.ce-plot-field-btn {
  flex: 0 0 190px;
}

.ce-plot-controls .form-group {
  margin-bottom: 6px;
}

.ce-plot-controls .form-control,
.ce-plot-controls .selectize-input,
.ce-plot-controls .selectize-dropdown {
  border-radius: 9px !important;
  border-color: #d6dbe1 !important;
  min-height: 32px;
  font-size: 11.5px;
}

.ce-plot-controls label {
  font-size: 10.8px;
  letter-spacing: 0.01em;
  color: #4b5563;
  margin-bottom: 3px;
}

.ce-analysis-controls {
  display: flex;
  align-items: flex-end;
  gap: 8px;
  flex-wrap: wrap;
  padding: 2px 2px 8px;
  border-top: 1px solid var(--ce-surface-2);
  margin-top: 2px;
}

.ce-analysis-field {
  flex: 1 1 180px;
  min-width: 170px;
}

.ce-analysis-field.ce-analysis-field-btn {
  flex: 0 0 160px;
}

.ce-analysis-controls .form-group {
  margin-bottom: 6px;
}

.ce-analysis-controls .form-control,
.ce-analysis-controls .selectize-input,
.ce-analysis-controls .selectize-dropdown {
  border-radius: 9px !important;
  border-color: #d6dbe1 !important;
  min-height: 32px;
  font-size: 11.5px;
}

.ce-analysis-controls label {
  font-size: 10.8px;
  letter-spacing: 0.01em;
  color: #4b5563;
  margin-bottom: 3px;
}

.ce-analysis-overlay {
  position: fixed;
  inset: 0;
  z-index: 4200;
  background: rgba(15, 23, 42, 0.3);
  backdrop-filter: blur(1.5px);
  opacity: 0;
  pointer-events: none;
  transition: opacity 170ms ease;
}

.ce-analysis-drawer {
  position: fixed;
  top: 0;
  right: 0;
  bottom: 0;
  z-index: 4300;
  width: min(430px, 92vw);
  background: #ffffff;
  border-left: 1px solid var(--ce-border);
  box-shadow: -24px 0 40px rgba(15, 23, 42, 0.12);
  transform: translateX(104%);
  transition: transform 210ms cubic-bezier(0.2, 0.72, 0.2, 1);
  display: flex;
  flex-direction: column;
}

html.ce-analysis-open .ce-analysis-overlay {
  opacity: 1;
  pointer-events: auto;
}

html.ce-analysis-open .ce-analysis-drawer {
  transform: translateX(0);
}

.ce-analysis-drawer-head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  padding: 14px 14px 10px;
  border-bottom: 1px solid var(--ce-surface-2);
  background: #fbfcfd;
}

.ce-analysis-drawer-title h2 {
  margin: 0;
  font-size: 18px;
  line-height: 1.2;
  letter-spacing: -0.02em;
  color: #111827;
}

.ce-analysis-drawer .ce-eyebrow {
  margin-bottom: 5px;
}

.ce-analysis-drawer-body {
  padding: 12px 14px 14px;
  overflow-y: auto;
}

.ce-analysis-drawer-body .form-group {
  margin-bottom: 8px;
}

.ce-analysis-drawer-body .form-control,
.ce-analysis-drawer-body .selectize-input,
.ce-analysis-drawer-body .selectize-dropdown {
  border-radius: 9px !important;
  border-color: #d6dbe1 !important;
  min-height: 32px;
  font-size: 11.5px;
}

.ce-analysis-drawer-body label {
  font-size: 11px;
  color: #374151;
  margin-bottom: 3px;
}

.ce-analysis-drawer-body .ce-help {
  margin: 8px 0 12px;
}

.ce-export-grid {
  display: grid;
  grid-template-columns: 1fr;
  gap: 6px;
  padding-top: 2px;
}

.ce-panel details summary {
  cursor: pointer;
  font-size: 11px;
  color: var(--ce-muted);
  margin: 2px 4px 7px;
  user-select: none;
}

.ce-panel pre {
  border: 1px solid var(--ce-border);
  border-radius: 11px;
  background: #f8fafb;
  color: #1f2937;
  padding: 10px;
  font-size: 11px;
}

.ce-output-shell {
  position: relative;
  border: 1px solid transparent;
  border-radius: 11px;
  background: #ffffff;
  transition: border-color 150ms ease, box-shadow 150ms ease;
}

.ce-output-shell:focus-within {
  border-color: #d6dbe1;
  box-shadow: 0 0 0 3px rgba(17, 17, 17, 0.06);
}

.ce-output-shell .shiny-bound-output {
  position: relative;
  display: block;
  overflow: hidden;
  border-radius: 10px;
  transition: opacity 160ms ease, filter 160ms ease;
}

.ce-output-shell .shiny-bound-output.recalculating {
  opacity: 0.52;
  filter: saturate(0.88);
}

.ce-output-shell .shiny-bound-output.recalculating::after {
  content: \"\";
  position: absolute;
  inset: 0;
  border-radius: 10px;
  background: linear-gradient(
    102deg,
    rgba(249, 250, 251, 0.45) 8%,
    rgba(226, 232, 240, 0.35) 26%,
    rgba(249, 250, 251, 0.45) 44%
  );
  background-size: 220% 100%;
  animation: ce-shimmer 1.1s linear infinite;
  pointer-events: none;
}

.ce-output-shell .shiny-bound-output.recalculating::before {
  content: \"\";
  position: absolute;
  top: 12px;
  right: 12px;
  width: 13px;
  height: 13px;
  border: 2px solid #d1d5db;
  border-top-color: #111111;
  border-radius: 50%;
  animation: ce-spin 0.75s linear infinite;
  pointer-events: none;
  z-index: 2;
}

.ce-output-brain svg [data-id] {
  transition: opacity 120ms ease, stroke 120ms ease, fill 120ms ease;
}

@keyframes ce-shimmer {
  from {
    background-position: 220% 0;
  }
  to {
    background-position: -220% 0;
  }
}

@keyframes ce-spin {
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
  }
}

#brain_plot .girafe_container_std {
  transition: opacity 140ms ease;
}

#cluster_table .dataTables_wrapper .dataTables_filter input,
#cluster_table .dataTables_wrapper .dataTables_length select {
  border-radius: 9px;
  border: 1px solid #d1d5db;
  padding: 4px 8px;
  font-size: 11.5px;
}

#cluster_table .dataTables_wrapper .dataTables_info,
#cluster_table .dataTables_wrapper .dataTables_paginate {
  font-size: 11.5px;
  color: #4b5563;
}

#cluster_table table.dataTable thead th {
  background: var(--ce-surface);
  border-bottom: 1px solid var(--ce-border);
  color: #374151;
  font-size: 10.8px;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  font-weight: 610;
}

#cluster_table table.dataTable tbody td {
  font-size: 12px;
  border-top: 1px solid #f1f3f5;
  transition: background-color 140ms ease, color 140ms ease;
}

#cluster_table table.dataTable.hover tbody tr:hover,
#cluster_table table.dataTable.display tbody tr:hover {
  background-color: #f8fafc !important;
}

#cluster_table table.dataTable tbody tr.selected,
#cluster_table table.dataTable tbody tr.odd.selected,
#cluster_table table.dataTable tbody tr.even.selected {
  background-color: #eceff3 !important;
  color: #111827;
}

#cluster_table .dataTables_wrapper .dataTables_paginate .paginate_button {
  border-radius: 8px !important;
  border: 1px solid transparent !important;
  transition: all 120ms ease;
}

#cluster_table .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: #f8fafc !important;
  border-color: #d6dbe1 !important;
  color: #111827 !important;
}

.ce-busy #apply_btn {
  pointer-events: none;
  opacity: 0.95;
  position: relative;
  padding-right: 28px;
}

.ce-busy #apply_btn::after {
  content: \"\";
  position: absolute;
  right: 10px;
  top: calc(50% - 6px);
  width: 12px;
  height: 12px;
  border: 1.8px solid rgba(255, 255, 255, 0.5);
  border-top-color: #ffffff;
  border-radius: 50%;
  animation: ce-spin 0.7s linear infinite;
}

@media (max-width: 991px) {
  .ce-shell {
    padding: 11px 8px 14px;
  }
  .ce-topbar {
    flex-direction: column;
    align-items: stretch;
    padding: 10px;
  }
  .ce-actions {
    width: 100%;
    justify-content: flex-start;
  }
  .ce-controls {
    position: static;
    margin-bottom: 11px;
  }
  .ce-plot-field,
  .ce-plot-field.ce-plot-field-btn,
  .ce-analysis-field,
  .ce-analysis-field.ce-analysis-field-btn {
    flex: 1 1 100%;
    min-width: 100%;
  }
  .ce-title-wrap h1 {
    font-size: 19px;
  }
  .ce-subtitle {
    font-size: 12px;
  }
  .ce-analysis-drawer {
    width: calc(100vw - 14px);
  }
}
"
}

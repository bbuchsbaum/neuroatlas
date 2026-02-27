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

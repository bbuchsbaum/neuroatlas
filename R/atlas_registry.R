#' Atlas Registry
#'
#' @description
#' A lightweight registry that maps canonical atlas ids and aliases to their
#' loader functions. The registry makes it possible to:
#'
#' * enumerate available atlases ([list_atlases()]),
#' * look up an atlas spec by id or alias ([find_atlas_spec()]), and
#' * dispatch to the correct loader with a single entry point ([get_atlas()]).
#'
#' Loaders are referenced by name (character string) rather than by function so
#' that registering a new atlas doesn't require the loader to already be
#' exported at package load time.
#'
#' @name atlas_registry
#' @keywords internal
NULL


# Package-local environment that stores atlas specs keyed by canonical id.
.neuroatlas_atlas_registry <- new.env(parent = emptyenv())


#' Register an Atlas Spec
#'
#' Adds (or replaces) an entry in the internal atlas registry.
#'
#' Registered as an internal helper — external packages that want to plug a
#' new atlas into [get_atlas()] dispatch can call this from their `.onLoad()`
#' hook.
#'
#' @param id Canonical atlas id (character scalar).
#' @param label Human-readable label.
#' @param family Atlas family (e.g. `"schaefer"`, `"glasser"`).
#' @param loader Name of the loader function (character scalar); resolved via
#'   [getExportedValue()] / [get()] at dispatch time.
#' @param default_space Default template space the loader returns when given no
#'   explicit `outspace` argument.
#' @param representation One of `"volume"`, `"surface"`, or `"derived"`.
#' @param aliases Optional character vector of accepted aliases.
#' @param description Optional free-text description.
#'
#' @return Invisibly, the spec list.
#' @keywords internal
register_atlas <- function(id,
                           label,
                           family,
                           loader,
                           default_space = NA_character_,
                           representation = c("volume", "surface", "derived"),
                           aliases = character(),
                           description = NA_character_) {
  representation <- match.arg(representation)
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    cli::cli_abort(
      "{.arg id} must be a non-empty character scalar.",
      class = c("neuroatlas_error_invalid_spec", "neuroatlas_error")
    )
  }
  if (!is.character(loader) || length(loader) != 1L || !nzchar(loader)) {
    cli::cli_abort(
      "{.arg loader} must be a non-empty character scalar naming a function.",
      class = c("neuroatlas_error_invalid_spec", "neuroatlas_error")
    )
  }

  spec <- list(
    id = as.character(id),
    label = as.character(label),
    family = as.character(family),
    loader = as.character(loader),
    default_space = as.character(default_space),
    representation = representation,
    aliases = as.character(aliases),
    description = as.character(description)
  )
  class(spec) <- c("atlas_spec", "list")

  assign(spec$id, spec, envir = .neuroatlas_atlas_registry)
  invisible(spec)
}


#' List Registered Atlases
#'
#' Enumerate the atlases currently registered in the neuroatlas dispatch
#' registry. Useful for discovery from scripts, vignettes, and Shiny apps.
#'
#' @return A tibble with one row per registered atlas and columns `id`,
#'   `label`, `family`, `representation`, `default_space`, and a comma-joined
#'   `aliases` string.
#'
#' @examples
#' list_atlases()
#'
#' @seealso [get_atlas()] to load an atlas by id.
#' @export
list_atlases <- function() {
  ids <- ls(.neuroatlas_atlas_registry)
  if (length(ids) == 0L) {
    return(tibble::tibble(
      id = character(),
      label = character(),
      family = character(),
      representation = character(),
      default_space = character(),
      aliases = character()
    ))
  }

  specs <- lapply(ids, function(i) get(i, envir = .neuroatlas_atlas_registry))
  tibble::tibble(
    id = vapply(specs, `[[`, character(1), "id"),
    label = vapply(specs, `[[`, character(1), "label"),
    family = vapply(specs, `[[`, character(1), "family"),
    representation = vapply(specs, `[[`, character(1), "representation"),
    default_space = vapply(specs, `[[`, character(1), "default_space"),
    aliases = vapply(specs, function(s) paste(s$aliases, collapse = ","),
                     character(1))
  )
}


#' Look Up an Atlas Spec by ID or Alias
#'
#' @param name Character scalar; an id or alias, matched case/underscore-insensitively.
#'
#' @return The matching `atlas_spec` list.
#' @keywords internal
find_atlas_spec <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort(
      "{.arg name} must be a non-empty character scalar.",
      class = c("neuroatlas_error_unknown_atlas", "neuroatlas_error")
    )
  }

  clean <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  target <- clean(name)

  ids <- ls(.neuroatlas_atlas_registry)
  for (id in ids) {
    spec <- get(id, envir = .neuroatlas_atlas_registry)
    candidates <- c(spec$id, spec$aliases)
    if (target %in% clean(candidates)) {
      return(spec)
    }
  }

  available <- if (length(ids) > 0L) {
    paste(ids, collapse = ", ")
  } else {
    "<none registered>"
  }
  cli::cli_abort(
    c(
      "Unknown atlas {.val {name}}.",
      "i" = "Available: {available}."
    ),
    class = c("neuroatlas_error_unknown_atlas", "neuroatlas_error")
  )
}


#' Load an Atlas by Registered ID
#'
#' Dispatches to the loader registered for `name`, forwarding `...` to it.
#' This is a thin convenience wrapper: it lets callers request an atlas by
#' string id (e.g. `"schaefer"`, `"glasser"`, `"aseg"`) without having to
#' know which specific `get_*_atlas()` function to call. Aliases registered
#' alongside the canonical id are matched case/punctuation-insensitively.
#'
#' @param name Atlas id or alias; see [list_atlases()] for available choices.
#' @param ... Arguments forwarded to the registered loader function.
#'
#' @return The loaded atlas object (class depends on the loader).
#'
#' @examples
#' \dontrun{
#' # Equivalent to get_aseg_atlas()
#' aseg <- get_atlas("aseg")
#'
#' # Equivalent to get_schaefer_atlas(parcels = "100", networks = "7")
#' schaef <- get_atlas("schaefer", parcels = "100", networks = "7")
#' }
#'
#' @seealso [list_atlases()].
#' @export
get_atlas <- function(name, ...) {
  spec <- find_atlas_spec(name)
  loader_fn <- tryCatch(
    getExportedValue("neuroatlas", spec$loader),
    error = function(e) {
      get(spec$loader, mode = "function", envir = asNamespace("neuroatlas"))
    }
  )
  loader_fn(...)
}


#' Populate the default registry.
#'
#' Invoked from [.onLoad()] so the registry is ready by the time user code runs.
#' @keywords internal
#' @noRd
.register_builtin_atlases <- function() {
  register_atlas(
    id = "schaefer",
    label = "Schaefer2018 cortical parcellation (volume)",
    family = "schaefer",
    loader = "get_schaefer_atlas",
    default_space = "MNI152NLin6Asym",
    representation = "volume",
    aliases = c("schaefer2018", "schaefer_volume")
  )
  register_atlas(
    id = "schaefer_surf",
    label = "Schaefer2018 cortical parcellation (surface)",
    family = "schaefer",
    loader = "schaefer_surf",
    default_space = "fsaverage6",
    representation = "surface",
    aliases = c("schaefer_surface")
  )
  register_atlas(
    id = "glasser",
    label = "Glasser HCP-MMP1.0 (volume)",
    family = "glasser",
    loader = "get_glasser_atlas",
    default_space = "MNI152NLin2009cAsym",
    representation = "volume",
    aliases = c("hcp-mmp", "hcp_mmp", "mmp1", "glasser360")
  )
  register_atlas(
    id = "glasser_surf",
    label = "Glasser HCP-MMP1.0 (surface)",
    family = "glasser",
    loader = "glasser_surf",
    default_space = "fsaverage",
    representation = "surface"
  )
  register_atlas(
    id = "aseg",
    label = "FreeSurfer ASEG subcortical atlas",
    family = "aseg",
    loader = "get_aseg_atlas",
    default_space = "MNI152NLin6Asym",
    representation = "volume",
    aliases = c("freesurfer_aseg")
  )
  register_atlas(
    id = "olsen_mtl",
    label = "Olsen MTL atlas",
    family = "olsen",
    loader = "get_olsen_mtl",
    default_space = "MNI152_custom",
    representation = "volume",
    aliases = c("olsen", "mtl")
  )
  register_atlas(
    id = "hippocampus",
    label = "Hippocampus (derived from Olsen MTL)",
    family = "olsen",
    loader = "get_hipp_atlas",
    default_space = "MNI152_custom",
    representation = "derived",
    aliases = c("hipp")
  )
  register_atlas(
    id = "subcortical",
    label = "Subcortical atlases (TemplateFlow)",
    family = "subcortical",
    loader = "get_subcortical_atlas",
    default_space = "MNI152NLin6Asym",
    representation = "volume",
    aliases = c("atlaspack")
  )
}


#' Print an atlas_spec
#'
#' @param x An `atlas_spec` object.
#' @param ... Unused.
#' @keywords internal
#' @export
print.atlas_spec <- function(x, ...) {
  cat("<atlas_spec>\n")
  cat("  id:             ", x$id, "\n", sep = "")
  cat("  family:         ", x$family, "\n", sep = "")
  cat("  loader:         ", x$loader, "()\n", sep = "")
  cat("  representation: ", x$representation, "\n", sep = "")
  cat("  default_space:  ", x$default_space, "\n", sep = "")
  if (length(x$aliases) > 0L) {
    cat("  aliases:        ", paste(x$aliases, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

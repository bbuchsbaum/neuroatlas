#' Atlas and Template Metadata
#'
#' A versioned, portable description of a loaded resource. Metadata are stored
#' with the object and can be inspected without downloading files or resolving
#' citations. Atlas objects store the record in `x$metadata`; template volumes
#' and surfaces use the `neuroatlas_metadata` attribute, preserving their class.
#'
#' @param x An atlas, or a template returned by [get_template()] or
#'   [load_surface_template()]. For validation, a `NeuroResourceMetadata` record.
#' @param ... Reserved for methods.
#' @return `atlas_metadata()` and `template_metadata()` return a
#'   `NeuroResourceMetadata` list with `schema_version`, `kind`, `identity`,
#'   `content`, `spatial`, `provenance`, `citations`, `artifacts`, `history`,
#'   and `parents`. `validate_resource_metadata()` returns its input invisibly.
#' @details
#' `identity` describes the resource and its published version (unknown versions
#' are `NA`). `content` describes the loaded representation, value type, selected
#' parameters and current region count. `spatial` describes the current sampling
#' geometry; `artifacts` retain source-space and resolution descriptors.
#' A TemplateFlow resolution key
#' is a query parameter, not a voxel size in millimetres.
#'
#' `spatial$basis` records whether template identity is source-declared,
#' inferred, user-supplied, or unknown. Geometry alone does not identify an
#' anatomical template. Grid resampling does not establish registration to a
#' different anatomical template.
#'
#' `citations` is a table with reference roles; [atlas_citations()] and
#' [template_citations()] convert it to R `bibentry` objects. `history` contains
#' structured parameters and software versions. `parents` retains metadata
#' snapshots for composite resources. Missing historical metadata in legacy
#' objects is reported conservatively; it is not reconstructed from the network.
#'
#' The older [atlas_ref()], [atlas_artifacts()] and [atlas_history()] accessors
#' read this authoritative record. Legacy list fields are compatibility copies;
#' do not edit them to change metadata. Metadata preservation is guaranteed for
#' neuroatlas loaders and supported atlas operations, not arbitrary operations
#' in other packages or direct replacement of an object's data.
#' @examples
#' a <- get_aseg_atlas()
#' atlas_metadata(a)
#' atlas_metadata(a)$spatial$voxel_size
#' atlas_citations(a)
#' @export
atlas_metadata <- function(x, ...) {
  UseMethod("atlas_metadata")
}

#' @rdname atlas_metadata
#' @export
atlas_metadata.atlas <- function(x, ...) {
  if (!is.null(x$metadata)) {
    validate_resource_metadata(x$metadata)
    return(x$metadata)
  }
  .build_atlas_metadata(x, legacy = TRUE)
}

#' @rdname atlas_metadata
#' @export
atlas_metadata.default <- function(x, ...) {
  meta <- attr(x, "neuroatlas_metadata", exact = TRUE)
  if (!is.null(meta) && identical(meta$kind, "atlas")) {
    validate_resource_metadata(meta)
    return(meta)
  }
  stop("Expected an atlas object; use template_metadata() for templates.")
}

#' @rdname atlas_metadata
#' @export
atlas_metadata.wang_prob_volumes <- function(x, ...) {
  validate_resource_metadata(x$metadata)
  x$metadata
}

#' @rdname atlas_metadata
#' @export
template_metadata <- function(x, ...) {
  meta <- attr(x, "neuroatlas_metadata", exact = TRUE)
  if (is.null(meta)) {
    stop("No template metadata attached to this object.")
  }
  validate_resource_metadata(meta)
  meta
}

#' @rdname atlas_metadata
#' @export
validate_resource_metadata <- function(x) {
  fail <- function(message) {
    cli::cli_abort(message, class = "neuroatlas_error_invalid_metadata")
  }
  required <- c("schema_version", "kind", "identity", "content", "spatial",
                "provenance", "citations", "artifacts", "history", "parents")
  if (!inherits(x, "NeuroResourceMetadata") ||
      !all(required %in% names(x))) fail("Incomplete resource metadata.")
  if (!identical(x$schema_version, 1L)) fail("Unsupported metadata schema.")
  choice <- function(value, choices) {
    is.character(value) && length(value) == 1L &&
      !is.na(value) && value %in% choices
  }
  if (!choice(x$kind, c("atlas", "template"))) fail("Invalid resource kind.")
  for (nm in c("identity", "content", "spatial", "provenance")) {
    if (!is.list(x[[nm]])) fail(paste0(nm, " must be a named list."))
  }
  for (nm in c("id", "name", "family", "model", "version", "description",
               "species", "coverage")) {
    value <- x$identity[[nm]]
    if (!is.character(value) || length(value) != 1L) {
      fail(paste0("identity$", nm, " must be a character scalar or NA."))
    }
  }
  if (is.na(x$identity$id) || !nzchar(x$identity$id)) {
    fail("A resource identifier is required.")
  }
  if (!choice(x$content$representation, c("volume", "surface"))) {
    fail("Representation must be volume or surface.")
  }
  if (!choice(x$content$value_type,
      c("labels", "probability", "intensity", "mask", "geometry", "unknown"))) {
    fail("Invalid resource value type.")
  }
  if (!choice(x$spatial$basis,
      c("source_declared", "inferred", "user_supplied", "unknown"))) {
    fail("Invalid spatial evidence basis.")
  }
  if (!is.list(x$content$parameters) ||
      !is.logical(x$content$derived) || length(x$content$derived) != 1L ||
      is.na(x$content$derived) || !is.numeric(x$content$regions) ||
      length(x$content$regions) != 1L ||
      (!is.na(x$content$regions) &&
       (!is.finite(x$content$regions) || x$content$regions < 0 ||
        x$content$regions != floor(x$content$regions)))) {
    fail("Invalid content parameters, derived status, or region count.")
  }
  for (nm in c("template_space", "coord_space", "resolution", "density")) {
    if (!is.character(x$spatial[[nm]]) || length(x$spatial[[nm]]) != 1L) {
      fail(paste0("spatial$", nm, " must be a character scalar or NA."))
    }
  }
  if (length(x$spatial$voxel_size) &&
      (!is.numeric(x$spatial$voxel_size) ||
       length(x$spatial$voxel_size) != 3L ||
       any(!is.finite(x$spatial$voxel_size) | x$spatial$voxel_size <= 0))) {
    fail("Voxel size must contain three positive finite values.")
  }
  if (!is.null(x$spatial$affine) &&
      (!is.numeric(x$spatial$affine) ||
       !identical(dim(x$spatial$affine), c(4L, 4L)) ||
       any(!is.finite(x$spatial$affine)))) fail("Invalid spatial affine.")
  if (!is.data.frame(x$citations) ||
      !all(names(.empty_resource_citations()) %in% names(x$citations))) {
    fail("Invalid citation table.")
  }
  if (!is.data.frame(x$artifacts) ||
      !all(names(.empty_atlas_artifacts()) %in% names(x$artifacts))) {
    fail("Invalid artifact table.")
  }
  if (!is.data.frame(x$history) ||
      !all(names(.empty_atlas_history()) %in% names(x$history)) ||
      !identical(x$history$step, seq_len(nrow(x$history))) ||
      !is.list(x$history$parameters) ||
      !all(vapply(x$history$parameters, is.list, logical(1)))) {
    fail("History must have consecutive step numbers and a valid schema.")
  }
  if (!is.list(x$parents)) fail("Parents must be metadata snapshots.")
  invisible(lapply(x$parents, validate_resource_metadata))
  invisible(x)
}

#' @rdname atlas_metadata
#' @export
print.NeuroResourceMetadata <- function(x, ...) {
  validate_resource_metadata(x)
  show <- function(value) {
    value <- as.character(value)
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value)) paste(unique(value), collapse = ", ") else "not recorded"
  }
  line <- function(label, value) {
    cat(sprintf("  %-14s %s\n", paste0(label, ":"), show(value)))
  }
  cat("<", x$kind, " metadata> ", x$identity$name, "\n", sep = "")
  line("Description", x$identity$description)
  line("Version", x$identity$version)
  line("Content", c(x$content$value_type, x$content$representation))
  if (!is.na(x$content$regions)) line("Regions", x$content$regions)
  if (length(x$content$parameters)) {
    line("Variant", vapply(names(x$content$parameters), function(nm) {
      values <- x$content$parameters[[nm]]
      paste0(nm, "=", paste(utils::head(values, 6), collapse = "/"),
             if (length(values) > 6L) paste0(" (+", length(values) - 6L, ")"))
    }, character(1)))
  }
  line("Template", x$spatial$template_space)
  line("Coord. space", x$spatial$coord_space)
  line("Space basis", x$spatial$basis)
  if (length(x$spatial$voxel_size)) {
    line("Voxel size", paste0(paste(x$spatial$voxel_size, collapse = " x "),
                              " ", show(x$spatial$units)))
  }
  if (length(x$spatial$vertex_count)) {
    line("Vertices", paste(names(x$spatial$vertex_count),
                           x$spatial$vertex_count, sep = "="))
    line("Surface", c(x$spatial$density, x$spatial$surface_type))
  }
  line("Source", x$provenance$source)
  licenses <- x$artifacts$license
  missing_license <- is.na(licenses) | !nzchar(licenses)
  if (any(missing_license) && any(!missing_license)) {
    licenses <- c(licenses[!missing_license],
                   paste(sum(missing_license), "artifact(s) not recorded"))
  }
  line("License", licenses)
  if (nrow(x$citations)) {
    ref <- x$citations[1L, ]
    label <- if (!is.na(ref$author) && !is.na(ref$year)) {
      paste0(sub(" and others$", " et al.", ref$author), " (", ref$year, ")")
    } else ref$title
    line("Citation", paste0("[", ref$role, "] ", label,
                             if (!is.na(ref$doi)) paste0("; doi:", ref$doi)))
    if (nrow(x$citations) > 1L) line("Citations", paste(nrow(x$citations), "total"))
  } else {
    line("Citation", NA_character_)
  }
  actions <- x$history$action[x$history$action != "load"]
  line("Modifications", if (length(actions)) actions else "none recorded")
  if (length(x$provenance$issues)) line("Notes", x$provenance$issues)
  invisible(x)
}

.metadata_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) {
    default
  } else as.character(x[[1]])
}

.resource_geometry <- function(x, representation) {
  spatial <- list(
    dimensions = integer(), voxel_size = numeric(), affine = NULL,
    units = NA_character_, vertex_count = integer(), hemisphere = character(),
    surface_type = NA_character_
  )
  if (representation == "volume" &&
      (methods::is(x, "NeuroVol") || methods::is(x, "ClusteredNeuroVol"))) {
    sp <- neuroim2::space(x)
    spatial$dimensions <- as.integer(dim(x))
    spatial$voxel_size <- as.numeric(neuroim2::spacing(sp))[1:3]
    spatial$affine <- neuroim2::trans(sp)
    spatial$units <- "mm"
  } else if (representation == "surface") {
    surfaces <- if (is.list(x) && !isS4(x)) x else list(surface = x)
    counts <- vapply(surfaces, function(s) {
      tryCatch(as.integer(nrow(neurosurf::vertices(s))),
               error = function(e) NA_integer_)
    }, integer(1))
    spatial$vertex_count <- counts[!is.na(counts)]
    spatial$hemisphere <- names(spatial$vertex_count)
  }
  spatial
}

.new_resource_metadata <- function(kind, identity, content, spatial,
                                   provenance, citations, artifacts, history,
                                   parents = list()) {
  history <- .normalize_atlas_history(history)
  history$step <- seq_len(nrow(history))
  ret <- structure(list(
    schema_version = 1L, kind = kind, identity = identity, content = content,
    spatial = spatial, provenance = provenance, citations = citations,
    artifacts = .normalize_atlas_artifacts(artifacts), history = history,
    parents = parents
  ), class = c("NeuroResourceMetadata", "list"))
  validate_resource_metadata(ret)
  ret
}

.build_atlas_metadata <- function(x, legacy = FALSE) {
  ref <- x$atlas_ref
  if (is.null(ref)) ref <- atlas_ref(x)
  catalog <- if (legacy) NULL else .atlas_catalog_entry(ref$family, ref$model)
  representation <- if (inherits(x, "surfatlas")) "surface" else "volume"
  data <- if (representation == "surface") {
    list(left = x$lh_atlas, right = x$rh_atlas)
  } else x$atlas
  spatial <- c(list(
    template_space = ref$template_space, coord_space = ref$coord_space,
    resolution = ref$resolution, density = ref$density,
    basis = if (legacy || is.na(ref$template_space)) "unknown" else {
      if (ref$confidence == "uncertain" ||
          ref$template_space %in% c("MNI152", "MNI152_custom",
                                    "MNI152_unspecified")) "inferred" else {
        "source_declared"
      }
    }
  ), .resource_geometry(data, representation))
  spatial$surface_type <- .metadata_scalar(x$surf_type)
  if (length(spatial$voxel_size)) {
    spatial$resolution <- paste0(paste(spatial$voxel_size, collapse = "x"), "mm")
    if (length(unique(spatial$voxel_size)) == 1L) {
      spatial$resolution <- paste0(spatial$voxel_size[[1]], "mm")
    }
  }
  artifacts <- .normalize_atlas_artifacts(x$atlas_artifacts %||%
                                          .empty_atlas_artifacts())
  history <- .normalize_atlas_history(x$atlas_history %||% .empty_atlas_history())
  if (any(history$action == "resample") ||
      identical(ref$template_space, "custom")) {
    spatial$basis <- "user_supplied"
  }
  params <- x$metadata_parameters %||% list()
  for (nm in c("parcels", "networks", "threshold", "type", "template_atlas")) {
    value <- x[[nm]]
    if (is.null(value) && nm %in% names(artifacts)) {
      values <- unique(artifacts[[nm]])
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values) == 1L) value <- values
    }
    if (!is.null(value)) params[[nm]] <- value
  }
  issues <- character()
  if (legacy) issues <- "Legacy object: source metadata were not captured."
  if (is.null(catalog) || !nrow(catalog$citations)) {
    issues <- c(issues, "Original atlas publication has not been verified.")
  }
  citations <- .resource_citations(artifacts, catalog)
  if (representation == "surface") {
    for (hemi in c("left", "right")) {
      surface <- data[[hemi]]
      receipts <- attr(surface, "neuroatlas_surface_sources", exact = TRUE)
      if (is.null(receipts)) next
      idx <- which(artifacts$hemi == hemi &
                     grepl("annotation|label_overlay", artifacts$role))
      if (length(idx)) {
        for (nm in names(receipts$annotation)) {
          artifacts[[nm]][idx] <- receipts$annotation[[nm]]
        }
      }
      geometry <- receipts$geometry
      if (!is.null(geometry)) {
        artifacts <- artifacts[!grepl("geometry", artifacts$role) |
                                 (!is.na(artifacts$hemi) &
                                    artifacts$hemi != hemi), ]
        geometry$artifacts$role <- paste0("geometry_", hemi)
        geometry$artifacts$hemi <- hemi
        artifacts <- dplyr::bind_rows(artifacts, geometry$artifacts)
        citations <- .deduplicate_citations(
          dplyr::bind_rows(citations, geometry$citations))
      }
      if (!is.null(receipts$bundled_geometry)) {
        artifacts <- dplyr::bind_rows(artifacts, receipts$bundled_geometry)
      }
    }
  }
  .new_resource_metadata(
    kind = "atlas",
    identity = list(
      id = paste(ref$family, ref$model, sep = ":"),
      name = .metadata_scalar(x$name, ref$model),
      family = ref$family, model = ref$model,
      version = .metadata_scalar(catalog$version),
      description = .metadata_scalar(catalog$description),
      species = .metadata_scalar(catalog$species),
      coverage = .metadata_scalar(catalog$coverage)
    ),
    content = list(representation = representation, value_type = "labels",
                   parameters = params, regions = length(x$ids),
                   derived = identical(ref$representation, "derived")),
    spatial = spatial,
    provenance = list(source = ref$source, url = ref$provenance,
                      lineage = ref$lineage, confidence = ref$confidence,
                      notes = ref$notes, issues = issues,
                      package_version = as.character(utils::packageVersion("neuroatlas"))),
    citations = citations, artifacts = artifacts, history = history
  )
}

.metadata_ref <- function(meta) {
  new_atlas_ref(
    family = meta$identity$family, model = meta$identity$model,
    representation = if (isTRUE(meta$content$derived)) "derived" else {
      meta$content$representation
    },
    template_space = meta$spatial$template_space,
    coord_space = meta$spatial$coord_space,
    resolution = meta$spatial$resolution, density = meta$spatial$density,
    provenance = meta$provenance$url, source = meta$provenance$source,
    lineage = meta$provenance$lineage, confidence = meta$provenance$confidence,
    notes = meta$provenance$notes
  )
}

.store_atlas_metadata <- function(x, meta) {
  validate_resource_metadata(meta)
  x$metadata <- meta
  x$atlas_ref <- .metadata_ref(meta)
  x$atlas_artifacts <- meta$artifacts
  x$atlas_history <- meta$history
  x$space <- x$template_space <- meta$spatial$template_space
  x$coord_space <- meta$spatial$coord_space
  x$confidence <- meta$provenance$confidence
  x$roi_metadata <- .build_roi_metadata(x)
  x
}

.refresh_atlas_metadata <- function(x, meta = atlas_metadata(x)) {
  data <- if (inherits(x, "surfatlas")) {
    list(left = x$lh_atlas, right = x$rh_atlas)
  } else x$atlas
  geometry <- .resource_geometry(data, meta$content$representation)
  for (nm in setdiff(names(geometry), "surface_type")) {
    meta$spatial[[nm]] <- geometry[[nm]]
  }
  meta$content$regions <- length(x$ids)
  .store_atlas_metadata(x, meta)
}

.enrich_atlas_metadata <- function(meta, inputs) {
  meta$content$parameters <- utils::modifyList(
    meta$content$parameters, inputs$parameters %||% list())
  source <- inputs$source
  if (!is.null(source)) {
    validate_resource_metadata(source)
    # Replace generic TemplateFlow image descriptors with the resolved artifact.
    keep <- !(meta$artifacts$source_name == "TemplateFlow" &
                meta$artifacts$role %in%
                c("parcellation_volume", "summary_label_volume"))
    keep[is.na(keep)] <- TRUE
    resolved <- source$artifacts
    descriptor <- meta$artifacts[!keep, ]
    if (nrow(descriptor) == 1L) {
      for (nm in c("role", "family", "model", "variant", "lineage", "notes")) {
        resolved[[nm]] <- descriptor[[nm]][[1]]
      }
    }
    meta$artifacts <- dplyr::bind_rows(meta$artifacts[keep, ], resolved)
    meta$citations <- .deduplicate_citations(
      dplyr::bind_rows(meta$citations, source$citations))
    meta$provenance$issues <- unique(c(meta$provenance$issues,
                                      source$provenance$issues))
    meta$parents$source_template <- source
  }
  if (length(inputs$parents)) {
    meta$parents <- c(meta$parents, inputs$parents)
    for (parent in inputs$parents) {
      meta$citations <- .deduplicate_citations(
        dplyr::bind_rows(meta$citations, parent$citations))
      meta$artifacts <- dplyr::bind_rows(meta$artifacts, parent$artifacts)
    }
  }
  processing <- inputs$processing
  if (!is.null(processing)) {
    idx <- which(meta$history$action == "resample")
    if (!length(idx)) {
      row <- .new_atlas_history(
        "resample", "volume", confidence = "approximate",
        details = "Resampled the sampling grid; no anatomical registration applied.")
      # Load precedes resampling; subsequent derivation steps follow it.
      pos <- sum(meta$history$action == "load")
      meta$history <- dplyr::bind_rows(
        meta$history[seq_len(pos), ], row,
        meta$history[which(seq_len(nrow(meta$history)) > pos), ])
      idx <- pos + 1L
    }
    native <- meta$history$from_template_space[which(
      !is.na(meta$history$from_template_space))[1]]
    if (!length(native) || is.na(native)) native <- NA_character_
    target <- meta$spatial$template_space
    meta$spatial$sampling_reference <- target
    meta$spatial$template_space <- native
    meta$spatial$basis <- if (is.na(native)) "unknown" else "inferred"
    for (i in idx) {
      meta$history$parameters[[i]] <- c(processing,
                                      list(target_grid_template = target))
      meta$history$from_template_space[[i]] <- native
      meta$history$to_template_space[[i]] <- native
    }
    meta$history$to_template_space[meta$history$action == "load"] <- native
    meta$provenance$issues <- unique(c(meta$provenance$issues,
      "Grid resampling does not establish registration to another template."))
  }
  meta$history$step <- seq_len(nrow(meta$history))
  meta
}

.finish_resource_resample <- function(x, source_metadata, processing) {
  attr(x, "neuroatlas_processing") <- processing
  if (!is.null(source_metadata)) {
    meta <- source_metadata
    geometry <- .resource_geometry(x, "volume")
    for (nm in names(geometry)) meta$spatial[[nm]] <- geometry[[nm]]
    meta$spatial$resolution <- paste0(paste(geometry$voxel_size,
                                           collapse = "x"), "mm")
    row <- .new_atlas_history(
      "resample", "volume", from_template_space = meta$spatial$template_space,
      to_template_space = meta$spatial$template_space,
      details = "Changed sampling grid without anatomical registration.",
      parameters = processing)
    meta$history <- dplyr::bind_rows(meta$history, row)
    meta$history$step <- seq_len(nrow(meta$history))
    attr(x, "neuroatlas_metadata") <- meta
  }
  x
}

.capture_surface_sources <- function(x, annotation, geometry = NULL,
                                      bundled_geometry = NULL) {
  attr(x, "neuroatlas_surface_sources") <- list(
    annotation = .file_receipt(annotation),
    geometry = if (is.null(geometry)) NULL else {
      attr(geometry, "neuroatlas_metadata", exact = TRUE)
    }, bundled_geometry = bundled_geometry
  )
  x
}

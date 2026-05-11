#' FSL Atlas Loaders
#'
#' @description
#' Load FSL-distributed atlases into the standard neuroatlas `atlas` object
#' shape. FSL atlases are described by XML files under
#' `$FSLDIR/data/atlases`; many probabilistic atlases provide both a 4D
#' probability image and a 3D maximum-probability summary image. The summary
#' image labels are offset by one relative to XML/probability-volume indices,
#' so `get_fsl_atlas()` applies that correction when loading max-probability
#' summaries.
#'
#' @param name Atlas identifier, FSL XML path, or known alias. Known aliases
#'   include `"harvard_oxford_cortical"`, `"harvard_oxford_subcortical"`,
#'   `"harvard_oxford_cortical_subcortical"`, and `"julich"`.
#' @param fsl_dir FSL installation directory. Defaults to `Sys.getenv("FSLDIR")`.
#'   `get_julich_brain_atlas()` downloads an FSL-style Julich-Brain cache when
#'   this is empty and `download = TRUE`.
#' @param resolution Preferred image resolution, e.g. `"1mm"` or `"2mm"`.
#'   If `NULL`, the first image entry in the XML file is used.
#' @param image One of `"summary"` or `"probability"`. `neuroatlas` atlas
#'   objects are discrete parcellations, so `"summary"` is the default.
#'   `"probability"` currently returns paths and metadata when
#'   `path_only = TRUE`; loading 4D probabilistic images as atlas objects is
#'   intentionally deferred.
#' @param outspace Optional `NeuroSpace` to resample the discrete atlas into.
#' @param path_only Logical; return resolved paths and parsed metadata without
#'   loading image data.
#'
#' @return An `atlas` object, or a path/metadata list when `path_only = TRUE`.
#' @export
get_fsl_atlas <- function(name,
                          fsl_dir = Sys.getenv("FSLDIR"),
                          resolution = NULL,
                          image = c("summary", "probability"),
                          outspace = NULL,
                          path_only = FALSE) {
  image <- match.arg(image)
  spec <- .match_fsl_atlas_spec(name)
  xml_path <- .resolve_fsl_xml(name, spec = spec, fsl_dir = fsl_dir)
  meta <- .parse_fsl_atlas_xml(xml_path)
  img <- .select_fsl_image(meta$images, resolution = resolution)

  atlas_path <- if (identical(image, "summary")) {
    img$summaryimagefile
  } else {
    img$imagefile
  }

  if (is.na(atlas_path) || !nzchar(atlas_path) || !file.exists(atlas_path)) {
    cli::cli_abort(
      c(
        "Could not resolve FSL atlas image for {.val {name}}.",
        "i" = "XML: {.path {xml_path}}",
        "i" = "Requested image: {.val {image}}."
      ),
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }

  if (identical(image, "probability") && !path_only) {
    cli::cli_abort(
      c(
        "Probabilistic FSL atlas images are not discrete atlas objects.",
        "i" = "Use {.code image = \"summary\"} or {.code path_only = TRUE}."
      ),
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }

  if (path_only) {
    ret <- list(
      name = meta$name,
      shortname = meta$shortname,
      type = meta$type,
      xml_path = xml_path,
      atlas_path = atlas_path,
      labels = meta$labels,
      resolution = img$resolution,
      image = image
    )
    class(ret) <- c("fsl_atlas_paths", "list")
    return(ret)
  }

  vol <- neuroim2::read_vol(atlas_path)
  template_space <- .template_space_from_outspace(
    outspace,
    default_space = "MNI152"
  )
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace, smooth = FALSE)
  }

  values <- sort(unique(as.integer(as.vector(vol))))
  values <- values[values != 0L]
  labels <- .labels_for_fsl_values(meta$labels, values, meta$type, image)
  label_map <- as.list(labels$ids)
  names(label_map) <- labels$labels

  clustered <- neuroim2::ClusteredNeuroVol(
    as.logical(vol),
    clusters = as.numeric(vol[vol != 0]),
    label_map = label_map
  )

  family <- spec$family %||% .clean_fsl_id(meta$shortname)
  model <- spec$model %||% meta$shortname
  res_label <- img$resolution %||% NA_character_

  ref <- new_atlas_ref(
    family = family,
    model = model,
    representation = "volume",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = res_label,
    provenance = "FSL atlas XML and image files",
    source = "fsl",
    lineage = paste0("Loaded FSL XML atlas ", basename(xml_path), "."),
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = if (identical(meta$type, "probabilistic") &&
                identical(image, "summary")) {
      "Loaded maximum-probability summary image; XML label indices were shifted by one."
    } else {
      NA_character_
    }
  )

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "atlas_xml",
      family = family,
      model = model,
      source_name = "FSL",
      source_url = xml_path,
      source_ref = basename(xml_path),
      file_name = basename(xml_path),
      template_space = "MNI152",
      coord_space = "MNI152",
      resolution = res_label,
      lineage = "FSL atlas descriptor.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = if (identical(image, "summary")) {
        "summary_label_volume"
      } else {
        "probability_volume"
      },
      family = family,
      model = model,
      source_name = "FSL",
      source_url = atlas_path,
      source_ref = basename(atlas_path),
      file_name = basename(atlas_path),
      template_space = "MNI152",
      coord_space = "MNI152",
      resolution = res_label,
      lineage = paste0("Resolved from ", basename(xml_path), "."),
      confidence = "high"
    )
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = "MNI152",
    to_template_space = template_space,
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = if (is.null(outspace)) "high" else "approximate",
    details = paste0("Loaded FSL atlas ", meta$name, ".")
  )

  new_atlas(
    name = meta$name,
    atlas = clustered,
    ids = labels$ids,
    labels = labels$labels,
    orig_labels = labels$orig_labels,
    hemi = .infer_hemi(labels$labels),
    cmap = NULL,
    subclass = c(family, "fsl_atlas"),
    extra = list(
      fsl_shortname = meta$shortname,
      fsl_type = meta$type,
      fsl_xml = xml_path
    ),
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}

#' Load a Harvard-Oxford Atlas
#'
#' @description
#' Loads Harvard-Oxford cortical, subcortical, or combined cortical/subcortical
#' parcellations. By default this uses TemplateFlow, which does not require a
#' local FSL installation. Set `source = "fsl"` to read from `$FSLDIR`.
#'
#' @param type One of `"cortical"`, `"subcortical"`, or
#'   `"cortical_subcortical"`.
#' @param threshold Maximum-probability threshold, one of `0`, `25`, or `50`.
#' @param template_space TemplateFlow space.
#' @param resolution TemplateFlow/FSL resolution. TemplateFlow accepts `"01"`
#'   or `"02"`; FSL accepts values such as `"1mm"` and `"2mm"`.
#' @param source `"templateflow"` or `"fsl"`.
#' @param outspace Optional `NeuroSpace` to resample the atlas into.
#' @param use_cache Passed through to `get_template()`.
#' @param path_only Return resolved paths and metadata without loading image
#'   data.
#'
#' @param download Logical; for `get_julich_brain_atlas()`, download the
#'   Julich-Brain atlas archive into the neuroatlas cache when `fsl_dir` is
#'   unset.
#'
#' @return An `atlas` object, or path metadata when `path_only = TRUE`.
#' @export
get_harvard_oxford_atlas <- function(type = c("cortical", "subcortical",
                                              "cortical_subcortical"),
                                     threshold = c(25, 0, 50),
                                     template_space = "MNI152NLin6Asym",
                                     resolution = "01",
                                     source = c("templateflow", "fsl"),
                                     outspace = NULL,
                                     use_cache = TRUE,
                                     path_only = FALSE) {
  type <- match.arg(type)
  threshold <- as.integer(match.arg(as.character(threshold),
                                    choices = c("25", "0", "50")))
  source <- match.arg(source)

  if (identical(source, "fsl")) {
    fsl_resolution <- if (identical(resolution, "01") ||
                          identical(resolution, "1")) {
      "1mm"
    } else if (identical(resolution, "02") || identical(resolution, "2")) {
      "2mm"
    } else {
      resolution
    }
    return(get_fsl_atlas(
      name = paste0("harvard_oxford_", type),
      resolution = fsl_resolution,
      image = "summary",
      outspace = outspace,
      path_only = path_only
    ))
  }

  atlas_id <- switch(
    type,
    cortical = "HOCPA",
    subcortical = "HOSPA",
    cortical_subcortical = "HOCPAL"
  )
  desc <- paste0("th", threshold)

  tf_query <- list(
    space = template_space,
    variant = "dseg",
    atlas = atlas_id,
    desc = desc,
    suffix = "dseg",
    resolution = resolution,
    use_cache = use_cache
  )

  label_query <- tf_query
  label_query$desc <- NULL
  label_query["resolution"] <- list(NULL)
  label_query$extension <- ".tsv"
  label_query$path_only <- TRUE
  label_path <- tryCatch(
    do.call(get_template, label_query),
    error = function(e) {
      cli::cli_warn(
        c(
          "TemplateFlow label table for Harvard-Oxford atlas {.val {atlas_id}} was not available.",
          "i" = "{conditionMessage(e)}"
        ),
        class = c("neuroatlas_warn_missing_labels", "neuroatlas_warn"),
        .frequency = "once",
        .frequency_id = paste0("missing_labels:", atlas_id)
      )
      NULL
    }
  )

  if (path_only) {
    atlas_path <- do.call(get_template,
                          c(tf_query, list(path_only = TRUE)))
    ret <- list(
      name = paste("Harvard-Oxford", gsub("_", " ", type)),
      atlas_path = atlas_path,
      label_path = label_path,
      template_space = template_space,
      resolution = resolution,
      threshold = threshold,
      template_atlas = atlas_id
    )
    class(ret) <- c("harvard_oxford_paths", "list")
    return(ret)
  }

  vol <- do.call(get_template, c(tf_query, list(path_only = FALSE)))
  template_out <- .template_space_from_outspace(outspace, template_space)
  if (!is.null(outspace)) {
    vol <- resample(vol, outspace, smooth = FALSE)
  }

  ids <- sort(unique(as.integer(as.vector(vol))))
  ids <- ids[ids != 0L]
  label_info <- .read_label_table(label_path, ids)
  label_map <- as.list(ids)
  names(label_map) <- label_info$labels

  clustered <- neuroim2::ClusteredNeuroVol(
    as.logical(vol),
    clusters = as.numeric(vol[vol != 0]),
    label_map = label_map
  )

  family <- "harvard_oxford"
  model <- switch(
    type,
    cortical = "HarvardOxfordCortical",
    subcortical = "HarvardOxfordSubcortical",
    cortical_subcortical = "HarvardOxfordCorticalSubcortical"
  )
  atlas_name <- switch(
    type,
    cortical = "Harvard-Oxford cortical structural atlas",
    subcortical = "Harvard-Oxford subcortical structural atlas",
    cortical_subcortical = "Harvard-Oxford cortical/subcortical structural atlas"
  )

  ref <- new_atlas_ref(
    family = family,
    model = model,
    representation = "volume",
    template_space = template_out,
    coord_space = "MNI152",
    resolution = paste0(as.integer(resolution), "mm"),
    provenance = "TemplateFlow Harvard-Oxford atlas",
    source = "templateflow",
    lineage = paste0("TemplateFlow atlas=", atlas_id,
                     ", desc=", desc, "."),
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = "Maximum-probability Harvard-Oxford dseg image."
  )

  artifacts <- .new_atlas_artifact(
    role = "summary_label_volume",
    family = family,
    model = model,
    variant = atlas_id,
    source_name = "TemplateFlow",
    source_url = "https://www.templateflow.org",
    source_ref = paste0("atlas-", atlas_id, "_desc-", desc, "_dseg"),
    template_space = template_space,
    coord_space = "MNI152",
    resolution = paste0(as.integer(resolution), "mm"),
    lineage = paste0("TemplateFlow atlas=", atlas_id, "."),
    confidence = "high",
    notes = paste0("Threshold ", threshold, "% maximum-probability dseg.")
  )
  if (!is.null(label_path)) {
    artifacts <- dplyr::bind_rows(
      artifacts,
      .new_atlas_artifact(
        role = "label_table",
        family = family,
        model = model,
        variant = atlas_id,
        source_name = "TemplateFlow",
        source_url = "https://www.templateflow.org",
        source_ref = basename(label_path),
        file_name = basename(label_path),
        template_space = template_space,
        coord_space = "MNI152",
        resolution = paste0(as.integer(resolution), "mm"),
        confidence = "high"
      )
    )
  }

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = template_space,
    to_template_space = template_out,
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = if (is.null(outspace)) "high" else "approximate",
    details = paste0("Loaded TemplateFlow Harvard-Oxford atlas ", atlas_id,
                     " at threshold ", threshold, ".")
  )

  new_atlas(
    name = atlas_name,
    atlas = clustered,
    ids = ids,
    labels = label_info$labels,
    orig_labels = label_info$labels,
    hemi = .infer_hemi(label_info$labels),
    cmap = label_info$cmap,
    subclass = c("harvard_oxford", "fsl_atlas"),
    extra = list(
      type = type,
      threshold = threshold,
      template_atlas = atlas_id
    ),
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}

#' @rdname get_harvard_oxford_atlas
#' @export
get_harvard_oxford_cortical_atlas <- function(...) {
  get_harvard_oxford_atlas(type = "cortical", ...)
}

#' @rdname get_harvard_oxford_atlas
#' @export
get_harvard_oxford_subcortical_atlas <- function(...) {
  get_harvard_oxford_atlas(type = "subcortical", ...)
}

#' @rdname get_harvard_oxford_atlas
#' @export
get_harvard_oxford_cortical_subcortical_atlas <- function(...) {
  get_harvard_oxford_atlas(type = "cortical_subcortical", ...)
}

#' Load a Julich-Brain FSL Atlas
#'
#' @description
#' Thin wrapper around `get_fsl_atlas()` for the FSL-distributed Julich-Brain
#' cytoarchitectonic atlas. This requires a local FSL-style atlas directory and
#' uses the XML/image files under `$FSLDIR/data/atlases`.
#'
#' @inheritParams get_fsl_atlas
#'
#' @return An `atlas` object, or path metadata when `path_only = TRUE`.
#' @export
get_julich_brain_atlas <- function(fsl_dir = Sys.getenv("FSLDIR"),
                                   download = TRUE,
                                   ...) {
  if (!nzchar(fsl_dir) && isTRUE(download)) {
    fsl_dir <- .ensure_julich_brain_fsl_cache()
  }
  get_fsl_atlas("julich", fsl_dir = fsl_dir, ...)
}

#' @keywords internal
#' @noRd
.fsl_atlas_specs <- function() {
  tibble::tibble(
    id = c(
      "harvard_oxford_cortical",
      "harvard_oxford_subcortical",
      "harvard_oxford_cortical_subcortical",
      "julich"
    ),
    family = c(
      rep("harvard_oxford", 3),
      "julich_brain"
    ),
    model = c(
      "HarvardOxfordCortical",
      "HarvardOxfordSubcortical",
      "HarvardOxfordCorticalSubcortical",
      "JulichBrain"
    ),
    xml_file = c(
      "HarvardOxford-Cortical.xml",
      "HarvardOxford-Subcortical.xml",
      NA_character_,
      "Juelich.xml"
    ),
    shortname = c("HOCPA", "HOSPA", "HOCPAL", "Juelich"),
    aliases = list(
      c("ho_cortical", "harvard_oxford_cort", "harvard-oxford-cortical",
        "hocpa"),
      c("ho_subcortical", "harvard_oxford_sub", "harvard-oxford-subcortical",
        "hospa"),
      c("ho", "harvard_oxford", "harvard-oxford", "hocpal"),
      c("juelich", "julich_brain", "julichbrain", "brodmann")
    )
  )
}

#' @keywords internal
#' @noRd
.match_fsl_atlas_spec <- function(name) {
  specs <- .fsl_atlas_specs()
  target <- .clean_fsl_id(name)
  idx <- vapply(seq_len(nrow(specs)), function(i) {
    target %in% .clean_fsl_id(c(specs$id[i], specs$shortname[i],
                                specs$aliases[[i]]))
  }, logical(1))
  if (any(idx)) {
    return(specs[which(idx)[1], ])
  }
  list(family = NULL, model = NULL, xml_file = NULL, shortname = NULL)
}

#' @keywords internal
#' @noRd
.resolve_fsl_xml <- function(name, spec, fsl_dir = Sys.getenv("FSLDIR")) {
  if (file.exists(name)) {
    return(normalizePath(name))
  }
  if (!nzchar(fsl_dir)) {
    cli::cli_abort(
      c(
        "FSLDIR is not set and {.arg name} is not an XML path.",
        "i" = "Set {.envvar FSLDIR} or pass an explicit XML file path."
      ),
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }

  atlas_dir <- file.path(fsl_dir, "data", "atlases")
  if (!dir.exists(atlas_dir)) {
    cli::cli_abort(
      "FSL atlas directory does not exist: {.path {atlas_dir}}.",
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }

  if (!is.null(spec$xml_file) && !is.na(spec$xml_file) &&
      nzchar(spec$xml_file)) {
    direct <- file.path(atlas_dir, spec$xml_file)
    if (file.exists(direct)) {
      return(normalizePath(direct))
    }
  }

  xmls <- list.files(atlas_dir, pattern = "\\.xml$", full.names = TRUE)
  target <- .clean_fsl_id(c(name, spec$shortname %||% character()))
  for (xml in xmls) {
    meta <- tryCatch(.parse_fsl_atlas_xml(xml, labels = FALSE),
                     error = function(e) NULL)
    if (is.null(meta)) next
    candidates <- .clean_fsl_id(c(basename(xml), meta$name, meta$shortname))
    if (any(target %in% candidates)) {
      return(normalizePath(xml))
    }
  }

  cli::cli_abort(
    c(
      "Could not find FSL atlas XML for {.val {name}}.",
      "i" = "Searched: {.path {atlas_dir}}."
    ),
    class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
  )
}

#' @keywords internal
#' @noRd
.parse_fsl_atlas_xml <- function(xml_path, labels = TRUE) {
  txt <- paste(readLines(xml_path, warn = FALSE), collapse = "\n")
  atlas_dir <- dirname(xml_path)
  type <- tolower(.xml_first_tag(txt, "type") %||% "label")
  images <- .parse_fsl_images(txt, atlas_dir)
  label_tbl <- if (labels) .parse_fsl_labels(txt) else tibble::tibble()
  list(
    name = .xml_first_tag(txt, "name") %||% tools::file_path_sans_ext(basename(xml_path)),
    shortname = .xml_first_tag(txt, "shortname") %||%
      tools::file_path_sans_ext(basename(xml_path)),
    type = type,
    images = images,
    labels = label_tbl
  )
}

#' @keywords internal
#' @noRd
.parse_fsl_images <- function(txt, atlas_dir) {
  blocks <- .regex_matches(txt, "<images[^>]*>[\\s\\S]*?</images>")
  if (length(blocks) == 0L) {
    cli::cli_abort(
      "FSL atlas XML did not contain any {.tag images} blocks.",
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }
  tibble::tibble(
    imagefile = unname(vapply(blocks, function(x) {
      .resolve_fsl_image_path(.xml_first_tag(x, "imagefile"), atlas_dir)
    }, character(1))),
    summaryimagefile = unname(vapply(blocks, function(x) {
      path <- .xml_first_tag(x, "summaryimagefile")
      if (is.null(path)) path <- .xml_first_tag(x, "imagefile")
      .resolve_fsl_image_path(path, atlas_dir)
    }, character(1))),
    resolution = unname(vapply(blocks, .infer_fsl_resolution, character(1)))
  )
}

#' @keywords internal
#' @noRd
.parse_fsl_labels <- function(txt) {
  matches <- .regex_matches(txt, "<label\\b[^>]*>[\\s\\S]*?</label>")
  if (length(matches) == 0L) {
    return(tibble::tibble(
      index = integer(),
      name = character(),
      x = numeric(),
      y = numeric(),
      z = numeric()
    ))
  }

  rows <- lapply(matches, function(x) {
    attrs <- .parse_xml_attrs(sub("^<label\\b([^>]*)>.*$", "\\1", x))
    body <- sub("^<label\\b[^>]*>(.*?)</label>$", "\\1", x, perl = TRUE)
    tibble::tibble(
      index = as.integer(.xml_attr(attrs, "index")),
      name = .xml_unescape(body),
      x = as.numeric(.xml_attr(attrs, "x")),
      y = as.numeric(.xml_attr(attrs, "y")),
      z = as.numeric(.xml_attr(attrs, "z"))
    )
  })
  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
.select_fsl_image <- function(images, resolution = NULL) {
  if (is.null(resolution)) {
    return(images[1, ])
  }
  target <- .clean_fsl_id(resolution)
  idx <- .clean_fsl_id(images$resolution) == target
  if (!any(idx)) {
    cli::cli_abort(
      c(
        "FSL atlas resolution {.val {resolution}} is not available.",
        "i" = "Available: {.val {images$resolution}}."
      ),
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }
  images[which(idx)[1], ]
}

#' @keywords internal
#' @noRd
.labels_for_fsl_values <- function(label_tbl, values, type, image) {
  ids <- label_tbl$index
  if (identical(tolower(type), "probabilistic") &&
      identical(image, "summary")) {
    ids <- ids + 1L
  }

  if (!all(values %in% ids) && all(values %in% label_tbl$index)) {
    ids <- label_tbl$index
  }

  keep <- ids %in% values
  ids <- ids[keep]
  labels <- label_tbl$name[keep]

  missing <- setdiff(values, ids)
  if (length(missing) > 0L) {
    ids <- c(ids, missing)
    labels <- c(labels, paste0("Region", missing))
  }

  ord <- order(ids)
  tibble::tibble(
    ids = as.integer(ids[ord]),
    labels = as.character(labels[ord]),
    orig_labels = as.character(labels[ord])
  )
}

#' @keywords internal
#' @noRd
.xml_first_tag <- function(txt, tag) {
  pat <- paste0("<", tag, "[^>]*>(.*?)</", tag, ">")
  m <- regexec(pat, txt, perl = TRUE)
  out <- regmatches(txt, m)[[1]]
  if (length(out) < 2L) {
    return(NULL)
  }
  .xml_unescape(trimws(out[2]))
}

#' @keywords internal
#' @noRd
.parse_xml_attrs <- function(txt) {
  matches <- .regex_matches(txt, "([A-Za-z_:][-A-Za-z0-9_:.]*)\\s*=\\s*\"([^\"]*)\"")
  if (length(matches) == 0L) {
    return(character())
  }
  keys <- sub("\\s*=.*$", "", matches)
  vals <- sub("^[^=]+=\"([^\"]*)\"$", "\\1", matches)
  vals <- vapply(vals, .xml_unescape, character(1))
  stats::setNames(vals, keys)
}

#' @keywords internal
#' @noRd
.xml_attr <- function(attrs, name) {
  if (name %in% names(attrs)) attrs[[name]] else NA_character_
}

#' @keywords internal
#' @noRd
.regex_matches <- function(txt, pattern) {
  m <- gregexpr(pattern, txt, perl = TRUE)
  out <- regmatches(txt, m)[[1]]
  if (length(out) == 1L && identical(out, character(0))) {
    character()
  } else {
    out
  }
}

#' @keywords internal
#' @noRd
.xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

#' @keywords internal
#' @noRd
.resolve_fsl_image_path <- function(path, atlas_dir) {
  if (is.null(path) || !nzchar(path)) {
    return(NA_character_)
  }
  path <- sub("^/+", "", path)
  candidates <- c(
    file.path(atlas_dir, path),
    paste0(file.path(atlas_dir, path), ".nii.gz"),
    paste0(file.path(atlas_dir, path), ".nii")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0L) {
    normalizePath(hit[1])
  } else {
    candidates[1]
  }
}

#' @keywords internal
#' @noRd
.infer_fsl_resolution <- function(block) {
  path <- paste(c(.xml_first_tag(block, "imagefile"),
                  .xml_first_tag(block, "summaryimagefile")),
                collapse = " ")
  res <- regmatches(path, regexpr("[0-9]+mm", path, perl = TRUE))
  if (length(res) == 0L || !nzchar(res)) {
    return(NA_character_)
  }
  res
}

#' @keywords internal
#' @noRd
.clean_fsl_id <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as.character(x)))
}

#' @keywords internal
#' @noRd
.ensure_julich_brain_fsl_cache <- function() {
  fsl_dir <- .neuroatlas_cache_dir("fsl")
  xml_path <- file.path(fsl_dir, "data", "atlases", "Juelich.xml")
  if (file.exists(xml_path)) {
    return(fsl_dir)
  }

  archive <- file.path(fsl_dir, "Juelich.tgz")
  .neuroatlas_download(
    url = "https://www.nitrc.org/frs/download.php/12096/Juelich.tgz",
    dest = archive,
    mode = "wb",
    quiet = TRUE,
    min_size = 1024L,
    description = "Julich-Brain atlas archive"
  )

  utils::untar(archive, exdir = fsl_dir, tar = "internal")
  if (!file.exists(xml_path)) {
    cli::cli_abort(
      c(
        "Downloaded Julich-Brain archive did not unpack to the expected FSL atlas layout.",
        "i" = "Expected: {.path {xml_path}}."
      ),
      class = c("neuroatlas_error_fsl_atlas", "neuroatlas_error")
    )
  }
  fsl_dir
}

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

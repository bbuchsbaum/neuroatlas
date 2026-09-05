# Capture TemplateFlow descriptions once, while loading. Reading metadata from
# a loaded object never calls this function. Failure to obtain a description
# leaves an explicit issue, without making an otherwise readable image unusable.
.template_description <- function(template) {
  tryCatch(templateflow::tf_get_metadata(template = template),
           error = function(e) list(
             metadata_issue = paste("Template description unavailable:",
                                    conditionMessage(e))))
}

.template_citation_table <- function(description) {
  refs <- unlist(description$ReferencesAndLinks, use.names = FALSE)
  ret <- .empty_resource_citations()
  for (ref in refs) {
    doi <- if (grepl("^(https?://(dx\\.)?doi.org/)?10\\.", ref)) {
      sub("^https?://(dx\\.)?doi.org/", "", ref)
    } else NA_character_
    ret <- dplyr::bind_rows(ret, .resource_citation(
      title = ref, doi = doi, role = "template",
      url = if (grepl("^https?://", ref)) ref else NA_character_
    ))
  }
  ret <- dplyr::bind_rows(ret, .resource_citation(
    "TemplateFlow: FAIR-sharing of multi-scale, multi-species brain models",
    "10.1038/s41592-022-01681-2", "Rastko Ciric and others", "2022",
    "Nature Methods", role = "software"
  ))
  .deduplicate_citations(ret)
}

.attach_template_metadata <- function(x, path, template, query,
                                      representation = "volume",
                                      description = .template_description(template)) {
  geometry <- .resource_geometry(x, representation)
  suffix <- .metadata_scalar(query$suffix)
  is_atlas_file <- !is.na(.metadata_scalar(query$atlas))
  value_type <- if (representation == "surface") "geometry" else {
    switch(suffix, dseg = "labels", probseg = "probability", mask = "mask",
           "intensity")
  }
  coord <- suppressWarnings(template_to_coord_space(template))
  spatial <- c(list(
    template_space = template, coord_space = coord,
    resolution = NA_character_, density = .metadata_scalar(query$density),
    basis = "source_declared"
  ), geometry)
  if (length(geometry$voxel_size)) {
    spatial$resolution <- paste0(paste(geometry$voxel_size, collapse = "x"), "mm")
    if (length(unique(geometry$voxel_size)) == 1L) {
      spatial$resolution <- paste0(geometry$voxel_size[[1]], "mm")
    }
  }
  if (representation == "surface") {
    spatial$surface_type <- suffix
    spatial$hemisphere <- .metadata_scalar(query$hemi)
  }
  artifacts <- .new_atlas_artifact(
    role = if (representation == "surface") "geometry" else "template_image",
    family = "template", model = template, source_name = "TemplateFlow",
    source_url = paste0("https://templateflow.s3.amazonaws.com/tpl-",
                        template, "/", basename(path)),
    source_ref = basename(path), file_name = basename(path), local_path = path,
    source_version = .metadata_scalar(description$Version),
    license = if (is_atlas_file) NA_character_ else {
      .metadata_scalar(description$License)
    },
    license_url = paste0("https://github.com/templateflow/tpl-", template),
    template_space = template, coord_space = coord,
    resolution = spatial$resolution, density = spatial$density,
    hemi = .metadata_scalar(query$hemi), confidence = "high"
  )
  history <- .new_atlas_history(
    "load", representation, from_template_space = template,
    to_template_space = template, from_coord_space = coord,
    to_coord_space = coord, confidence = "high",
    details = "Loaded the resolved TemplateFlow artifact.", parameters = query
  )
  issues <- description$metadata_issue %||% character()
  if (is_atlas_file) {
    issues <- c(issues,
      "Atlas-file license unverified; template terms may cover different data.")
  }
  if (!length(description$ReferencesAndLinks)) {
    issues <- c(issues, "Original template references were not supplied.")
  }
  meta <- .new_resource_metadata(
    kind = "template",
    identity = list(
      id = template, name = .metadata_scalar(description$Name, template),
      family = "template", model = template,
      version = .metadata_scalar(description$Version),
      description = .metadata_scalar(description$Description),
      species = .metadata_scalar(description$Species),
      coverage = NA_character_
    ),
    content = list(representation = representation, value_type = value_type,
                   parameters = query, regions = NA_integer_, derived = FALSE),
    spatial = spatial,
    provenance = list(source = "TemplateFlow", url = artifacts$source_url,
                      lineage = "TemplateFlow distributed resource.",
                      confidence = "high", notes = NA_character_, issues = issues,
                      package_version = as.character(utils::packageVersion("neuroatlas")),
                      templateflow_version =
                        .metadata_scalar(description$TemplateFlowVersion)),
    citations = .template_citation_table(description),
    artifacts = artifacts, history = history
  )
  attr(x, "neuroatlas_metadata") <- meta
  x
}

.attach_template_pair_metadata <- function(x) {
  left <- template_metadata(x$L)
  right <- template_metadata(x$R)
  meta <- left
  meta$spatial$vertex_count <- c(L = unname(left$spatial$vertex_count),
                                R = unname(right$spatial$vertex_count))
  meta$spatial$hemisphere <- c("L", "R")
  meta$content$parameters$hemi <- "both"
  meta$artifacts <- dplyr::bind_rows(left$artifacts, right$artifacts)
  meta$citations <- .deduplicate_citations(
    dplyr::bind_rows(left$citations, right$citations))
  meta$parents <- list(L = left, R = right)
  meta$history <- .new_atlas_history(
    "combine_hemispheres", "surface", parameters = list(hemispheres = c("L", "R")))
  meta$history$step <- 1L
  validate_resource_metadata(meta)
  attr(x, "neuroatlas_metadata") <- meta
  x
}

# Curated resource descriptions. Publication identifiers are independent of
# artifact releases: a paper's year is not an upstream data version.
.empty_resource_citations <- function() {
  tibble::tibble(role = character(), title = character(), author = character(),
                 year = character(), journal = character(), doi = character(),
                 url = character())
}

.resource_citation <- function(title, doi = NA_character_,
                               author = NA_character_, year = NA_character_,
                               journal = NA_character_, role = "atlas",
                               url = NA_character_) {
  if (is.na(url) && !is.na(doi)) url <- paste0("https://doi.org/", doi)
  tibble::tibble(role = role, title = title, author = author,
                 year = as.character(year), journal = journal, doi = doi,
                 url = url)
}

.atlas_catalog_entry <- function(family, model = NULL) {
  if (identical(family, "subcortical") && is.null(model)) {
    return(list(description = "AtlasPack harmonized subcortical parcellations.",
                 coverage = "subcortex and cerebellum", species = "Homo sapiens",
                 version = NA_character_, citations = .resource_citation(
                   "AtlasPack: harmonized neuroimaging atlases",
                   role = "distribution", url = "https://github.com/PennLINC/AtlasPack")))
  }
  key <- if (family == "subcortical") model else family
  if (identical(key, "visual")) key <- "julich_brain"
  entry <- switch(key,
    schaefer = list(
      description = "Functional cortical parcellation with network assignments.",
      coverage = "cerebral cortex",
      citations = .resource_citation(
        "Local-Global Parcellation of the Human Cerebral Cortex from Intrinsic Functional Connectivity MRI",
        "10.1093/cercor/bhx179", "Alexander Schaefer and others", "2018",
        "Cerebral Cortex")),
    glasser = list(
      description = "Glasser multi-modal parcellation of the cerebral cortex.",
      coverage = "cerebral cortex",
      citations = .resource_citation(
        "A multi-modal parcellation of human cerebral cortex",
        "10.1038/nature18933", "Matthew F. Glasser and others", "2016", "Nature")),
    aseg = list(
      description = "Bundled standard-space FreeSurfer subcortical labels.",
      coverage = "subcortex and brainstem",
      citations = .resource_citation(
        "Whole brain segmentation: automated labeling of neuroanatomical structures in the human brain",
        "10.1016/S0896-6273(02)00569-X", "Bruce Fischl and others", "2002",
        "Neuron")),
    olsen = list(
      description = "Bundled Olsen medial temporal lobe parcellation.",
      coverage = "medial temporal lobe",
      citations = .empty_resource_citations()),
    brainnetome = list(
      description = "Connectivity-based cortical and subcortical parcellation.",
      coverage = "cerebral cortex and subcortex",
      citations = .resource_citation(
        "The Human Brainnetome Atlas: A New Brain Atlas Based on Connectional Architecture",
        "10.1093/cercor/bhw157", "Lingzhong Fan and others", "2016",
        "Cerebral Cortex")),
    harvard_oxford = list(
      description = "Harvard-Oxford structural atlas, maximum-probability labels.",
      coverage = "cerebral cortex and/or subcortex",
      # References recommended by the FSL atlas distributor.
      citations = dplyr::bind_rows(
        .resource_citation(
          "Decreased volume of left and total anterior insular lobule in schizophrenia",
          author = "Nikos Makris and others", year = "2006",
          journal = "Schizophrenia Research"),
        .resource_citation(
          "Structural brain magnetic resonance imaging of limbic and thalamic volumes in pediatric bipolar disorder",
          author = "Jean A. Frazier and others", year = "2005",
          journal = "American Journal of Psychiatry"),
        .resource_citation(
          "An automated labeling system for subdividing the human cerebral cortex on MRI scans into gyral based regions of interest",
          author = "Rahul S. Desikan and others", year = "2006",
          journal = "NeuroImage"),
        .resource_citation(
          "Hypothalamic abnormalities in schizophrenia: sex effects and genetic vulnerability",
          author = "Jill M. Goldstein and others", year = "2007",
          journal = "Biological Psychiatry"),
        .resource_citation(
          "Harvard-Oxford atlases: distributor references and attribution",
          role = "distribution",
          url = "https://fsl.fmrib.ox.ac.uk/fsl/docs/other/datasets.html#harvard-oxford-cortical-and-subcortical-structural-atlases"))),
    julich_brain = list(
      description = "Cytoarchitectonic parcellation distributed through FSL.",
      coverage = "cytoarchitectonically mapped brain regions",
      citations = .resource_citation(
        "Julich-Brain: A 3D probabilistic atlas of the human brain's cytoarchitecture",
        "10.1126/science.abb4588", "Katrin Amunts and others", "2020", "Science")),
    wang = list(
      description = "Maximum-probability labels of visual topographic areas.",
      coverage = "visual cortex",
      citations = .resource_citation(
        "Probabilistic Maps of Visual Topography in Human Cortex",
        "10.1093/cercor/bhu277", "Liang Wang and others", "2015",
        "Cerebral Cortex")),
    visfatlas = list(
      description = "Maximum-probability functional visual cortex labels.",
      coverage = "occipito-temporal visual cortex",
      citations = .resource_citation(
        "A Probabilistic Functional Atlas of Human Occipito-Temporal Visual Cortex",
        "10.1093/cercor/bhaa246", "Mona Rosenke and others", "2021",
        "Cerebral Cortex")),
    cit168 = list(
      description = "AtlasPack harmonization of CIT168 subcortical labels.",
      coverage = "subcortical nuclei",
      citations = .resource_citation(
        "A high-resolution probabilistic in vivo atlas of human subcortical brain nuclei",
        "10.1038/sdata.2018.63", "Wolfgang M. Pauli and others", "2018",
        "Scientific Data")),
    hcp_thalamus = list(
      description = "AtlasPack harmonization of HCP thalamic nuclei labels.",
      coverage = "thalamus",
      citations = .resource_citation(
        "In-vivo probabilistic atlas of human thalamic nuclei based on diffusion-weighted magnetic resonance imaging",
        "10.1038/sdata.2018.270", "Elena Najdenovska and others", "2018",
        "Scientific Data")),
    mdtb10 = list(
      description = "AtlasPack harmonization of the MDTB cerebellar parcellation.",
      coverage = "cerebellum",
      citations = .resource_citation(
        "Functional boundaries in the human cerebellum revealed by a multi-domain task battery",
        "10.1038/s41593-019-0436-x", "Maedbh King and others", "2019",
        "Nature Neuroscience")),
    hcp_hippamyg = list(
      description = "AtlasPack harmonization of HCP hippocampus/amygdala labels.",
      coverage = "hippocampus and amygdala",
      citations = .resource_citation(
        "The minimal preprocessing pipelines for the Human Connectome Project",
        "10.1016/j.neuroimage.2013.04.127", "Matthew F. Glasser and others",
        "2013", "NeuroImage")),
    NULL
  )
  if (!is.null(entry)) {
    entry$species <- "Homo sapiens"
    entry$version <- NA_character_
    if (family == "subcortical") {
      entry$citations <- dplyr::bind_rows(entry$citations, .resource_citation(
        "AtlasPack: harmonized neuroimaging atlases", role = "distribution",
        url = "https://github.com/PennLINC/AtlasPack"))
    }
  }
  entry
}

.resource_citations <- function(artifacts, catalog = NULL) {
  ret <- catalog$citations %||% .empty_resource_citations()
  for (doi in unique(artifacts$citation_doi)) {
    if (is.na(doi) || !nzchar(doi) || doi %in% ret$doi) next
    title <- if (doi == "10.6084/m9.figshare.3498446") {
      "HCP-MMP1.0 projected onto fsaverage"
    } else paste0("Source reference: ", doi)
    ret <- dplyr::bind_rows(ret, .resource_citation(
      title, doi, role = "distribution"))
  }
  .deduplicate_citations(ret)
}

.deduplicate_citations <- function(x) {
  if (!nrow(x)) return(.empty_resource_citations())
  key <- ifelse(!is.na(x$doi) & nzchar(x$doi), tolower(x$doi), x$url)
  key[is.na(key)] <- x$title[is.na(key)]
  # Retain roles when a work has more than one function.
  x[!duplicated(paste(x$role, key)), , drop = FALSE]
}

#' Bibliographic References for an Atlas or Template
#'
#' Convert the stored citation table to R bibliography entries. These accessors
#' never resolve DOIs or contact a service. References without complete article
#' metadata remain `Misc` entries with their recorded title and URL.
#'
#' @param x An atlas or loaded template object.
#' @param role Optional citation role(s), such as `"atlas"`, `"distribution"`,
#'   `"template"`, or `"software"`. `NULL` includes all roles.
#' @return A `bibentry` vector, suitable for [utils::toBibtex()]. Each entry's
#'   note identifies its role, and its key is derived from the stored DOI, URL,
#'   or title. The table with separate role rows is available
#'   through [atlas_metadata()] or [template_metadata()].
#' @examples
#' refs <- atlas_citations(get_aseg_atlas())
#' refs
#' utils::toBibtex(refs)
#' @export
atlas_citations <- function(x, role = NULL) {
  .citations_as_bibentry(atlas_metadata(x)$citations, role)
}

#' @rdname atlas_citations
#' @export
template_citations <- function(x, role = NULL) {
  .citations_as_bibentry(template_metadata(x)$citations, role)
}

.citations_as_bibentry <- function(refs, role) {
  if (!is.null(role)) refs <- refs[refs$role %in% role, , drop = FALSE]
  result <- structure(list(), class = "bibentry")
  key <- ifelse(is.na(refs$doi), refs$url, tolower(refs$doi))
  key[is.na(key)] <- refs$title[is.na(key)]
  for (k in unique(key)) {
    rows <- refs[key == k, , drop = FALSE]
    fields <- as.list(rows[1, c("title", "author", "year", "journal",
                                "doi", "url")])
    fields <- fields[vapply(fields, function(v) !is.na(v) && nzchar(v), logical(1))]
    type <- if (all(c("author", "year", "journal") %in% names(fields))) {
      "Article"
    } else "Misc"
    fields$note <- paste("Role:", paste(unique(rows$role), collapse = ", "))
    fields$key <- paste0("neuroatlas_", gsub("[^[:alnum:]]+", "_", tolower(k)))
    result <- c(result, do.call(utils::bibentry, c(list(bibtype = type), fields)))
  }
  result
}

# License statements apply to the distributed artifact, never to every file
# merely because the neuroatlas package itself has an MIT license.
.artifact_license <- function(source, family) {
  if (identical(source, "CBIG")) {
    return(c(license = "MIT", license_url =
               "https://github.com/ThomasYeoLab/CBIG/blob/master/LICENSE.md"))
  }
  if (identical(source, "FSL") || identical(source, "Julich-Brain (FSL)")) {
    return(c(license = NA_character_, license_url =
               "https://fsl.fmrib.ox.ac.uk/fsl/docs/license.html"))
  }
  if (identical(source, "Brainnetome Center")) {
    return(c(license = NA_character_, license_url =
               "https://atlas.brainnetome.org/download.html"))
  }
  c(license = NA_character_, license_url = NA_character_)
}

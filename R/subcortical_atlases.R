#' Subcortical Atlas Options (TemplateFlow-backed)
#'
#' Returns a tibble describing the supported subcortical atlases built and
#' harmonized by the AtlasPack resource (HCP thalamus, MDTB10 cerebellum,
#' CIT168 subcortex, and HCP hippocampus/amygdala). Each row includes the
#' TemplateFlow atlas identifier, supported template spaces, and default
#' resolution/desc parameters used to fetch the data.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{id}: canonical neuroatlas identifier
#'   \item \code{atlas}: TemplateFlow \code{atlas} field
#'   \item \code{label}: human-readable name
#'   \item \code{default_space}: default TemplateFlow space
#'   \item \code{spaces}: list-column of allowed spaces
#'   \item \code{default_resolution}: default TemplateFlow resolution string
#'   \item \code{resolutions}: list-column of allowed resolutions
#'   \item \code{default_desc}: optional TemplateFlow \code{desc} value
#' }
#'
#' @references
#' 1. AtlasPack resource for harmonized atlases:
#'    \url{https://github.com/PennLINC/AtlasPack}
#' 2. Najdenovska E. et al. (2018) Scientific Data, HCP thalamus.
#' 3. King M. et al. (2019) Nature Neuroscience, MDTB10 cerebellum.
#' 4. Pauli W.M. et al. (2018) Scientific Data, CIT168 subcortex.
#' 5. Glasser M.F. et al. (2013) Neuroimage, HCP subcortical structures.
#'
#' @export
subcortical_atlas_options <- function() {
  .subcortical_specs()[, c("id", "atlas", "label", "default_space",
                           "spaces", "default_resolution",
                           "resolutions", "default_desc")]
}

#' Load harmonized subcortical atlases via TemplateFlow
#'
#' Fetches one of the AtlasPack-derived subcortical atlases (CIT168, HCP
#' thalamus, MDTB10 cerebellum, or HCP hippocampus/amygdala) using the existing
#' TemplateFlow integration. The returned object includes the source template
#' space and resolution so downstream workflows can track provenance.
#'
#' @param name Atlas identifier; see \code{\link{subcortical_atlas_options}()}.
#'   Aliases such as "thalamus_hcp" or "cerebellum" are accepted.
#' @param template_space TemplateFlow space to download (e.g.,
#'   \code{"MNI152NLin6Asym"} or \code{"MNI152NLin2009cAsym"}). Defaults to the
#'   atlas-specific recommended space.
#' @param resolution TemplateFlow resolution (e.g., "01"). Defaults to the atlas
#'   recommendation; validated against known options for the atlas.
#' @param desc Optional TemplateFlow \code{desc} to override the atlas default
#'   (e.g., "LRSplit" for CIT168 hemispheric split).
#' @param outspace Optional \code{NeuroSpace} or TemplateFlow query
#'   (string/list) to resample the atlas into a different space.
#' @param use_cache Logical; pass through to \code{\link{get_template}}.
#' @param path_only Logical; if TRUE, return paths to the atlas/label files plus
#'   metadata instead of loading into R objects.
#' @param ... Additional arguments forwarded to \code{\link{get_template}}.
#'
#' @return When \code{path_only=FALSE} (default), a list with classes
#'   \code{c("subcortical", "atlas")} containing:
#'   \itemize{
#'     \item \code{name}: human-friendly atlas name
#'     \item \code{atlas}: \code{ClusteredNeuroVol} with parcellation labels
#'     \item \code{ids}: integer vector of region IDs
#'     \item \code{labels}: character vector of region labels
#'     \item \code{orig_labels}: same as \code{labels}
#'     \item \code{hemi}: inferred hemisphere labels when available
#'     \item \code{cmap}: optional RGB mapping if provided by the label file
#'     \item \code{space}, \code{resolution}, \code{desc}: TemplateFlow metadata
#'     \item \code{template_atlas}: TemplateFlow \code{atlas} parameter used
#'   }
#'   When \code{path_only=TRUE}, a list with class
#'   \code{c("subcortical_paths","list")} containing \code{atlas_path},
#'   \code{label_path}, and the same metadata fields above.
#'
#' @examples
#' \dontrun{
#' # Load CIT168 in NLin6Asym space
#' cit <- get_subcortical_atlas("cit168", template_space = "MNI152NLin6Asym")
#'
#' # Get only the file paths without loading volumes
#' paths <- get_subcortical_atlas("mdtb10", path_only = TRUE)
#' }
#' @export
get_subcortical_atlas <- function(name,
                                  template_space = NULL,
                                  resolution = NULL,
                                  desc = NULL,
                                  outspace = NULL,
                                  use_cache = TRUE,
                                  path_only = FALSE,
                                  ...) {

  spec <- .match_subcortical_spec(name)

  # Resolve space and ensure it is supported
  tf_space <- if (is.null(template_space)) {
    spec$default_space
  } else {
    template_space
  }
  tf_space <- as.character(tf_space)
  if (!tf_space %in% spec$spaces[[1]]) {
    stop("TemplateFlow space '", tf_space, "' not supported for atlas '",
         spec$id, "'. Allowed: ", paste(spec$spaces[[1]], collapse = ", "))
  }

  # Resolve resolution within allowed set
  res <- if (is.null(resolution)) spec$default_resolution else resolution
  res <- as.character(res)
  if (!res %in% spec$resolutions[[1]]) {
    stop("Resolution '", res, "' not supported for atlas '", spec$id,
         "'. Allowed: ", paste(spec$resolutions[[1]], collapse = ", "))
  }

  tf_desc <- if (is.null(desc)) spec$default_desc else desc
  if (is.na(tf_desc)) {
    tf_desc <- NULL
  }

  # Prepare TemplateFlow query
  tf_query <- list(
    space = tf_space,
    atlas = spec$atlas,
    desc = tf_desc,
    suffix = "dseg",
    resolution = res,
    use_cache = use_cache
  )

  # Fetch label file path (may be NULL if not distributed)
  label_path <- .fetch_label_path(tf_query, ...)

  if (path_only) {
    atlas_path <- do.call(get_template,
                          c(tf_query, list(path_only = TRUE), list(...)))
    ret <- list(
      name = spec$label,
      atlas_path = atlas_path,
      label_path = label_path,
      space = tf_space,
      resolution = res,
      desc = tf_desc,
      template_atlas = spec$atlas
    )
    class(ret) <- c("subcortical_paths", "list")
    return(ret)
  }

  # Retrieve and optionally resample the atlas volume
  atlas_vol <- do.call(get_template,
                       c(tf_query, list(path_only = FALSE), list(...)))

  if (!is.null(outspace)) {
    resolved_space <- if (methods::is(outspace, "NeuroSpace")) {
      outspace
    } else {
      .resolve_template_input(outspace, target_type = "NeuroSpace")
    }
    atlas_vol <- resample(atlas_vol, resolved_space, smooth = FALSE)
  }

  # Build atlas metadata from labels/ids
  ids <- sort(unique(as.vector(atlas_vol[atlas_vol != 0])))
  label_info <- .read_label_table(label_path, ids)
  labels <- label_info$labels
  hemi <- .infer_hemi(labels)
  cmap <- label_info$cmap

  label_map <- as.list(ids)
  names(label_map) <- labels

  clustered <- neuroim2::ClusteredNeuroVol(
    as.logical(atlas_vol),
    clusters = as.numeric(atlas_vol[atlas_vol != 0]),
    label_map = label_map
  )

  ret <- list(
    name = spec$label,
    atlas = clustered,
    ids = ids,
    labels = labels,
    orig_labels = labels,
    hemi = hemi,
    cmap = cmap,
    network = NULL,
    space = tf_space,
    resolution = res,
    desc = tf_desc,
    template_atlas = spec$atlas
  )

  class(ret) <- c("subcortical", "atlas")
  ret
}

# Internal specification table -----------------------------------------------
.subcortical_specs <- function() {
  tibble::tibble(
    id = c("cit168", "hcp_thalamus", "mdtb10", "hcp_hippamyg"),
    atlas = c("CIT168", "hcpthalamic", "MDTB10", "HPandAMYG"),
    label = c("CIT168 subcortex (LR split)",
              "HCP thalamic nuclei",
              "MDTB10 cerebellum",
              "HCP hippocampus/amygdala"),
    default_space = rep("MNI152NLin6Asym", 4),
    spaces = list(
      c("MNI152NLin6Asym", "MNI152NLin2009cAsym"),
      c("MNI152NLin6Asym", "MNI152NLin2009cAsym"),
      c("MNI152NLin6Asym", "MNI152NLin2009cAsym"),
      c("MNI152NLin6Asym", "MNI152NLin2009cAsym")
    ),
    default_resolution = c("01", "01", "01", "01"),
    resolutions = list(
      "01",
      "01",
      "01",
      c("01", "06")
    ),
    default_desc = c("LRSplit", NA, NA, NA),
    aliases = list(
      c("cit", "cit_168", "cit-168"),
      c("hcpthalamus", "hcp_thalamus", "hcpthalamic", "thalamus_hcp"),
      c("cerebellum", "cerebellum_mdtb10", "diedrichsen", "mdtb-10"),
      c("hpandamyg", "hcp_hippamyg", "hippamyg", "hcp_hipp_amyg",
        "hippocampus_amygdala")
    )
  )
}

.match_subcortical_spec <- function(name) {
  specs <- .subcortical_specs()
  nm_clean <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  target <- nm_clean(name)

  idx <- vapply(seq_len(nrow(specs)), function(i) {
    target %in% nm_clean(c(specs$id[i], specs$aliases[[i]]))
  }, logical(1))

  if (!any(idx)) {
    stop("Unknown subcortical atlas '", name,
         "'. Available: ", paste(specs$id, collapse = ", "))
  }
  specs[idx, ]
}

.fetch_label_path <- function(tf_query, ...) {
  tryCatch({
    do.call(get_template,
            c(tf_query,
              list(extension = ".tsv", path_only = TRUE),
              list(...)))
  }, error = function(e) {
    NULL
  })
}

.read_label_table <- function(label_path, ids) {
  if (is.null(label_path) || !file.exists(label_path)) {
    labels <- paste0("Region", ids)
    return(list(labels = labels, cmap = NULL))
  }

  tbl <- tryCatch({
    utils::read.table(label_path, header = TRUE, sep = "\t",
                      stringsAsFactors = FALSE, comment.char = "",
                      check.names = FALSE)
  }, error = function(e) {
    NULL
  })

  if (is.null(tbl) || ncol(tbl) < 2) {
    labels <- paste0("Region", ids)
    return(list(labels = labels, cmap = NULL))
  }

  cn <- tolower(colnames(tbl))
  id_col <- if ("index" %in% cn) {
    tbl[[which(cn == "index")[1]]]
  } else if ("id" %in% cn) {
    tbl[[which(cn == "id")[1]]]
  } else {
    tbl[[1]]
  }

  label_col <- if ("label" %in% cn) {
    tbl[[which(cn == "label")[1]]]
  } else if ("name" %in% cn) {
    tbl[[which(cn == "name")[1]]]
  } else {
    tbl[[2]]
  }

  color_col <- if ("color" %in% cn) tbl[[which(cn == "color")[1]]] else NULL

  labels <- vapply(ids, function(i) {
    idx <- which(id_col == i)
    if (length(idx) == 0) {
      paste0("Region", i)
    } else {
      label_col[idx[1]]
    }
  }, character(1))

  cmap <- NULL
  if (!is.null(color_col)) {
    parse_rgb <- function(x) {
      parts <- strsplit(x, "x")[[1]]
      parts <- parts[parts != ""]
      if (length(parts) < 3) return(c(NA, NA, NA))
      as.numeric(parts[1:3])
    }
    rgb_mat <- t(vapply(color_col, parse_rgb, numeric(3)))
    cmap <- data.frame(
      red = rgb_mat[, 1],
      green = rgb_mat[, 2],
      blue = rgb_mat[, 3]
    )
  }

  list(labels = labels, cmap = cmap)
}

.infer_hemi <- function(labels) {
  hemi <- rep(NA_character_, length(labels))
  hemi[grepl("^(lh_|l_|left)", labels, ignore.case = TRUE) |
         grepl("_l$", labels, ignore.case = TRUE)] <- "left"
  hemi[grepl("^(rh_|r_|right)", labels, ignore.case = TRUE) |
         grepl("_r$", labels, ignore.case = TRUE)] <- "right"
  hemi
}

#' Brainnetome Atlas Download Sources
#'
#' @keywords internal
#' @noRd
.brainnetome_sources <- function() {
  base <- "https://pan.cstcloud.cn/s/api/shareDownload"
  data.frame(
    key = c("volume", "lut", "subregions", "networks"),
    file_name = c(
      "BN_Atlas_246_1mm.nii.gz",
      "BN_Atlas_246_LUT.txt",
      "BNA_subregions.xlsx",
      "subregion_func_network_Yeo_updated.csv"
    ),
    share_id = c("gfGflpp3Q0E", "Edvop4rRtU", "6eRCJ0zDTFk", "EYTnTX5SS5c"),
    fid = c("86217173499925", "86217173499928",
            "86217173499917", "86217173500082"),
    min_size = c(100000L, 1000L, 10000L, 1000L),
    stringsAsFactors = FALSE
  ) |>
    transform(
      url = paste0(base, "?shareId=", share_id, "&fid=", fid),
      page_url = paste0("https://pan.cstcloud.cn/s/", share_id)
    )
}

#' Download or Locate Cached Brainnetome Atlas Assets
#'
#' @keywords internal
#' @noRd
.brainnetome_asset_paths <- function(use_cache = TRUE) {
  src <- .brainnetome_sources()
  cache_dir <- .neuroatlas_cache_dir("brainnetome")
  paths <- stats::setNames(file.path(cache_dir, src$file_name), src$key)

  for (i in seq_len(nrow(src))) {
    key <- src$key[[i]]
    if (use_cache && file.exists(paths[[key]])) {
      next
    }
    .neuroatlas_download(
      url = src$url[[i]],
      dest = paths[[key]],
      min_size = src$min_size[[i]],
      description = paste0("Brainnetome ", key, " (", src$file_name[[i]], ")")
    )
  }

  paths
}

#' Brainnetome Atlas Label Table
#'
#' @description
#' Downloads (if needed) and returns the Brainnetome 246-region label table used
#' by \code{\link{get_brainnetome_atlas}()}.
#'
#' @details
#' Brainnetome assets are downloaded on demand and cached locally. They are not
#' bundled with neuroatlas; use is governed by the legal agreement on the
#' Brainnetome download page.
#'
#' @param use_cache Logical. Use cached Brainnetome files when available.
#'
#' @return A tibble with one row per parcel and columns for parcel id, label,
#'   hemisphere, lobe/gyrus, Yeo network membership, RGB colour, and
#'   cytoarchitectonic description.
#'
#' @source \url{https://atlas.brainnetome.org/download.html}
#'
#' @export
brainnetome_labels <- function(use_cache = TRUE) {
  paths <- .brainnetome_asset_paths(use_cache = use_cache)
  .read_brainnetome_labels(paths)
}

#' Load Brainnetome 246-Region Atlas
#'
#' @description
#' Downloads and loads the Brainnetome Atlas 246-region MNI152 1 mm labelmap.
#' Files are cached under the neuroatlas user cache directory rather than
#' bundled with the package.
#'
#' @details
#' The Brainnetome download page describes non-commercial use and attribution
#' terms. This loader keeps the data outside the package source and records the
#' upstream assets in \code{\link{atlas_artifacts}()}.
#'
#' @param outspace Optional \code{NeuroSpace} object or TemplateFlow-style
#'   outspace descriptor. If supplied, the atlas is resampled to that space.
#' @param smooth Logical. Whether to smooth parcel boundaries when resampling.
#' @param use_cache Logical. Use cached Brainnetome files when available.
#'
#' @return A list with classes \code{c("brainnetome", "volatlas", "atlas")}.
#'
#' @references
#' Fan, L. et al. (2016). The Human Brainnetome Atlas: A New Brain Atlas Based
#' on Connectional Architecture. Cerebral Cortex, 26(8), 3508-3526.
#'
#' @source \url{https://atlas.brainnetome.org/download.html}
#'
#' @importFrom neuroim2 read_vol ClusteredNeuroVol
#' @importFrom assertthat assert_that
#' @export
get_brainnetome_atlas <- function(outspace = NULL,
                                  smooth = FALSE,
                                  use_cache = TRUE) {
  template_space <- .template_space_from_outspace(
    outspace,
    default_space = "MNI152"
  )

  if (!is.null(outspace) && !methods::is(outspace, "NeuroSpace")) {
    outspace <- .resolve_template_input(outspace, target_type = "NeuroSpace")
    if (is.null(outspace) || !methods::is(outspace, "NeuroSpace")) {
      stop("'outspace' must resolve to a valid NeuroSpace object")
    }
  }

  paths <- .brainnetome_asset_paths(use_cache = use_cache)
  labels <- .read_brainnetome_labels(paths)
  vol <- neuroim2::read_vol(paths[["volume"]])

  if (!is.null(outspace)) {
    assertthat::assert_that(length(dim(outspace)) == 3)
    vol <- resample(vol, outspace, smooth)
  }

  actual_ids <- sort(unique(as.integer(vol[vol != 0])))
  if (length(actual_ids) == 0L) {
    stop("No Brainnetome atlas regions remain after resampling")
  }

  labels <- labels[match(actual_ids, labels$id), , drop = FALSE]
  if (anyNA(labels$id)) {
    stop("Brainnetome label table is missing ids present in the volume")
  }

  label_map <- as.list(actual_ids)
  names(label_map) <- labels$label
  vol <- neuroim2::ClusteredNeuroVol(
    as.logical(vol),
    clusters = vol[vol != 0],
    label_map = label_map
  )

  src <- .brainnetome_sources()
  vol_src <- src[src$key == "volume", , drop = FALSE]
  lut_src <- src[src$key == "lut", , drop = FALSE]
  sub_src <- src[src$key == "subregions", , drop = FALSE]
  net_src <- src[src$key == "networks", , drop = FALSE]

  ref <- new_atlas_ref(
    family = "brainnetome",
    model = "BrainnetomeAtlas246",
    representation = "volume",
    template_space = template_space,
    coord_space = "MNI152",
    resolution = "1mm",
    provenance = "https://atlas.brainnetome.org/download.html",
    source = "brainnetome_download",
    lineage = "Brainnetome Center MNI152 1mm maximum probability map.",
    confidence = if (is.null(outspace)) "high" else "approximate",
    notes = paste(
      "Runtime download from Brainnetome Center CAS Cloud share.",
      "Use is governed by the Brainnetome website legal agreement."
    )
  )

  artifacts <- dplyr::bind_rows(
    .new_atlas_artifact(
      role = "parcellation_volume",
      family = "brainnetome",
      model = "BrainnetomeAtlas246",
      source_name = "Brainnetome Center",
      source_url = vol_src$page_url,
      source_ref = vol_src$file_name,
      citation_doi = "10.1093/cercor/bhw157",
      file_name = vol_src$file_name,
      template_space = "MNI152",
      coord_space = "MNI152",
      resolution = "1mm",
      parcels = "246",
      lineage = "Brainnetome Center MNI152 1mm labelmap.",
      confidence = "high",
      notes = "Downloaded on demand; not bundled with neuroatlas."
    ),
    .new_atlas_artifact(
      role = "label_table",
      family = "brainnetome",
      model = "BrainnetomeAtlas246",
      source_name = "Brainnetome Center",
      source_url = lut_src$page_url,
      source_ref = lut_src$file_name,
      citation_doi = "10.1093/cercor/bhw157",
      file_name = lut_src$file_name,
      parcels = "246",
      lineage = "Brainnetome LUT label table.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "label_table",
      family = "brainnetome",
      model = "BrainnetomeAtlas246",
      source_name = "Brainnetome Center",
      source_url = sub_src$page_url,
      source_ref = sub_src$file_name,
      citation_doi = "10.1093/cercor/bhw157",
      file_name = sub_src$file_name,
      parcels = "246",
      lineage = "Brainnetome subregion and cytoarchitectonic descriptions.",
      confidence = "high"
    ),
    .new_atlas_artifact(
      role = "label_table",
      family = "brainnetome",
      model = "BrainnetomeAtlas246",
      source_name = "Brainnetome Center",
      source_url = net_src$page_url,
      source_ref = net_src$file_name,
      citation_doi = "10.1093/cercor/bhw157",
      file_name = net_src$file_name,
      parcels = "246",
      networks = "Yeo 7 / Yeo 17",
      lineage = "Brainnetome to Yeo network membership table.",
      confidence = "high"
    )
  )

  history <- .new_atlas_history(
    action = "load",
    representation = "volume",
    from_template_space = "MNI152",
    to_template_space = "MNI152",
    from_coord_space = "MNI152",
    to_coord_space = "MNI152",
    status = "available",
    confidence = "high",
    details = "Loaded Brainnetome 246-region MNI152 1mm atlas."
  )
  if (!is.null(outspace)) {
    history <- dplyr::bind_rows(
      history,
      .new_atlas_history(
        action = "resample",
        representation = "volume",
        from_template_space = "MNI152",
        to_template_space = template_space,
        from_coord_space = "MNI152",
        to_coord_space = "MNI152",
        status = "available",
        confidence = "approximate",
        details = paste0("Resampled Brainnetome atlas with smooth=", smooth, ".")
      )
    )
  }

  new_atlas(
    name = "Brainnetome-246",
    atlas = vol,
    ids = actual_ids,
    labels = labels$label,
    orig_labels = labels$label_full,
    hemi = labels$hemi,
    network = labels$yeo_17network_name,
    cmap = labels[, c("red", "green", "blue")],
    subclass = c("brainnetome", "volatlas"),
    extra = list(
      subregion_name = labels$subregion_name,
      region = labels$region,
      lobe = labels$lobe,
      gyrus = labels$gyrus,
      cytoarchitectonic_ref = labels$cytoarchitectonic_ref,
      mni_coord = labels$mni_coord,
      yeo_7network = labels$yeo_7network,
      yeo_7network_name = labels$yeo_7network_name,
      yeo_17network = labels$yeo_17network,
      yeo_17network_name = labels$yeo_17network_name
    ),
    ref = ref,
    artifacts = artifacts,
    history = history
  )
}

#' @keywords internal
#' @noRd
.read_brainnetome_labels <- function(paths) {
  lut <- utils::read.table(
    paths[["lut"]],
    header = FALSE,
    stringsAsFactors = FALSE,
    col.names = c("id", "label", "red", "green", "blue", "alpha")
  )
  lut <- lut[lut$id > 0L, , drop = FALSE]

  nets <- utils::read.csv(
    paths[["networks"]],
    skip = 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  nets <- nets[, c("Label", "subregion_name", "region",
                   "Yeo_7network", "Yeo_17network")]
  names(nets) <- c("id", "subregion_name", "region",
                   "yeo_7network", "yeo_17network")
  nets <- nets[!is.na(nets$id) & nets$id > 0L, , drop = FALSE]

  sub <- .read_brainnetome_subregions(paths[["subregions"]])

  out <- merge(lut, nets, by = "id", all.x = TRUE, sort = FALSE)
  out <- merge(out, sub, by = "id", all.x = TRUE, sort = FALSE)
  out <- out[order(out$id), , drop = FALSE]

  out$hemi <- ifelse(grepl("_L$", out$label), "left",
                     ifelse(grepl("_R$", out$label), "right", NA_character_))
  out$yeo_7network_name <- unname(.brainnetome_yeo7_map()[
    as.character(out$yeo_7network)
  ])
  out$yeo_17network_name <- unname(.brainnetome_yeo17_map()[
    as.character(out$yeo_17network)
  ])
  out$label_full <- paste0(out$region, ": ", out$cytoarchitectonic_ref)

  tibble::as_tibble(out[, c(
    "id", "label", "label_full", "hemi",
    "subregion_name", "region", "lobe", "gyrus",
    "cytoarchitectonic_ref", "mni_coord",
    "yeo_7network", "yeo_7network_name",
    "yeo_17network", "yeo_17network_name",
    "red", "green", "blue", "alpha"
  )])
}

#' @keywords internal
#' @noRd
.read_brainnetome_subregions <- function(path) {
  tmp <- tempfile("brainnetome_xlsx")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(
    path,
    files = c("xl/sharedStrings.xml", "xl/worksheets/sheet1.xml"),
    exdir = tmp
  )

  shared <- .xlsx_shared_strings(file.path(tmp, "xl/sharedStrings.xml"))
  sheet <- .xlsx_sheet_matrix(file.path(tmp, "xl/worksheets/sheet1.xml"),
                              shared, ncol = 9L)
  sheet <- sheet[-1L, , drop = FALSE]

  lobe <- .fill_down(sheet[, 1])
  gyrus <- .fill_down(sheet[, 2])
  region_lr <- sheet[, 3]
  id_left <- as.integer(sheet[, 4])
  id_right <- as.integer(sheet[, 5])
  cyto <- sheet[, 6]
  coord_left <- sheet[, 8]
  coord_right <- sheet[, 9]

  left <- data.frame(
    id = id_left,
    lobe = lobe,
    gyrus = gyrus,
    region_lr = region_lr,
    cytoarchitectonic_ref = cyto,
    mni_coord = coord_left,
    stringsAsFactors = FALSE
  )
  right <- data.frame(
    id = id_right,
    lobe = lobe,
    gyrus = gyrus,
    region_lr = region_lr,
    cytoarchitectonic_ref = cyto,
    mni_coord = coord_right,
    stringsAsFactors = FALSE
  )

  out <- rbind(left, right)
  out <- out[!is.na(out$id), , drop = FALSE]
  out$lobe <- trimws(out$lobe)
  out$gyrus <- trimws(out$gyrus)
  out$cytoarchitectonic_ref <- trimws(out$cytoarchitectonic_ref)
  out$mni_coord <- trimws(out$mni_coord)
  out[order(out$id), , drop = FALSE]
}

#' @keywords internal
#' @noRd
.brainnetome_yeo7_map <- function() {
  c(
    "1" = "Visual",
    "2" = "Somatomotor",
    "3" = "Dorsal Attention",
    "4" = "Ventral Attention",
    "5" = "Limbic",
    "6" = "Frontoparietal",
    "7" = "Default"
  )
}

#' @keywords internal
#' @noRd
.brainnetome_yeo17_map <- function() {
  c(
    "1" = "Visual peripheral",
    "2" = "Visual central",
    "3" = "Somato-motor A",
    "4" = "Somato-motor B",
    "5" = "Dorsal attention A",
    "6" = "Dorsal attention B",
    "7" = "Ventral attention",
    "8" = "Salience",
    "9" = "Limbic-1",
    "10" = "Limbic-2",
    "11" = "Control C",
    "12" = "Control A",
    "13" = "Control B",
    "14" = "Default D (Auditory)",
    "15" = "Default C",
    "16" = "Default A",
    "17" = "Default B"
  )
}

#' @keywords internal
#' @noRd
.fill_down <- function(x) {
  out <- x
  last <- NA_character_
  for (i in seq_along(out)) {
    if (!is.na(out[[i]]) && nzchar(out[[i]])) {
      last <- out[[i]]
    } else {
      out[[i]] <- last
    }
  }
  out
}

#' @keywords internal
#' @noRd
.xlsx_shared_strings <- function(path) {
  xml <- paste(readLines(path, warn = FALSE), collapse = "")
  items <- regmatches(xml, gregexpr("(?s)<si>.*?</si>", xml, perl = TRUE))[[1]]
  vapply(items, function(item) {
    parts <- regmatches(
      item,
      gregexpr("(?s)<t[^>]*>.*?</t>", item, perl = TRUE)
    )[[1]]
    txt <- gsub("(?s)^<t[^>]*>|</t>$", "", parts, perl = TRUE)
    .xml_unescape(paste(txt, collapse = ""))
  }, character(1))
}

#' @keywords internal
#' @noRd
.xlsx_sheet_matrix <- function(path, shared, ncol) {
  xml <- paste(readLines(path, warn = FALSE), collapse = "")
  rows <- regmatches(xml, gregexpr("(?s)<row[^>]*>.*?</row>", xml, perl = TRUE))[[1]]
  out <- matrix(NA_character_, nrow = length(rows), ncol = ncol)

  for (i in seq_along(rows)) {
    cells <- regmatches(
      rows[[i]],
      gregexpr("(?s)<c [^>]*>.*?</c>", rows[[i]], perl = TRUE)
    )[[1]]
    if (length(cells) == 0L || identical(cells, character(0))) {
      next
    }
    for (cell in cells) {
      ref <- sub('.* r="([A-Z]+)[0-9]+".*', "\\1", cell)
      col <- .xlsx_col_index(ref)
      if (is.na(col) || col < 1L || col > ncol) {
        next
      }
      value <- sub("(?s).*<v>(.*?)</v>.*", "\\1", cell, perl = TRUE)
      if (identical(value, cell)) {
        next
      }
      if (grepl(' t="s"', cell, fixed = TRUE)) {
        idx <- as.integer(value) + 1L
        value <- if (!is.na(idx) && idx <= length(shared)) shared[[idx]] else NA_character_
      }
      out[i, col] <- .xml_unescape(value)
    }
  }

  out
}

#' @keywords internal
#' @noRd
.xlsx_col_index <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  vals <- match(chars, LETTERS)
  if (anyNA(vals)) return(NA_integer_)
  sum(vals * 26L ^ rev(seq_along(vals) - 1L))
}

#' @keywords internal
#' @noRd
.xml_unescape <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  x
}

require_visual_qa_packages <- function() {
  for (package in c("neuroim2", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Visual QA requires package: ", package, call. = FALSE)
    }
  }
}

visual_edge_map <- function(volume, labels = FALSE) {
  values <- neuroim2::as.array(volume)
  if (length(dim(values)) != 3L) {
    stop("Visual QA edge input must be three-dimensional.", call. = FALSE)
  }
  present <- is.finite(values) & values != 0
  edges <- array(FALSE, dim(values))
  mark_change <- function(lower, upper) {
    left <- values[lower[[1L]], lower[[2L]], lower[[3L]], drop = FALSE]
    right <- values[upper[[1L]], upper[[2L]], upper[[3L]], drop = FALSE]
    changed <- if (isTRUE(labels)) {
      (left != right) & (left != 0 | right != 0)
    } else {
      xor(left != 0, right != 0)
    }
    edges[lower[[1L]], lower[[2L]], lower[[3L]]] <<-
      edges[lower[[1L]], lower[[2L]], lower[[3L]]] | changed
    edges[upper[[1L]], upper[[2L]], upper[[3L]]] <<-
      edges[upper[[1L]], upper[[2L]], upper[[3L]]] | changed
  }
  dimensions <- dim(values)
  if (dimensions[[1L]] > 1L) {
    mark_change(list(seq_len(dimensions[[1L]] - 1L), TRUE, TRUE),
                list(2:dimensions[[1L]], TRUE, TRUE))
  }
  if (dimensions[[2L]] > 1L) {
    mark_change(list(TRUE, seq_len(dimensions[[2L]] - 1L), TRUE),
                list(TRUE, 2:dimensions[[2L]], TRUE))
  }
  if (dimensions[[3L]] > 1L) {
    mark_change(list(TRUE, TRUE, seq_len(dimensions[[3L]] - 1L)),
                list(TRUE, TRUE, 2:dimensions[[3L]]))
  }
  edges <- edges & present
  neuroim2::NeuroVol(array(as.numeric(edges), dim = dim(edges)), neuroim2::space(volume))
}

write_visual_png <- function(path, draw) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(path, width = 2200, height = 1500, res = 180)
  device_open <- TRUE
  on.exit({
    if (isTRUE(device_open)) grDevices::dev.off()
  }, add = TRUE)
  draw()
  grDevices::dev.off()
  device_open <- FALSE
  if (!file.exists(path) || file.info(path)$size < 1024L) {
    stop("Visual QA plot was not written: ", path, call. = FALSE)
  }
  invisible(path)
}

render_visual_qa <- function(source_image,
                             target_image,
                             warped_image,
                             target_mask,
                             warped_mask,
                             target_labels,
                             warped_labels,
                             jacobian,
                             output_dir) {
  require_visual_qa_packages()
  source <- neuroim2::read_vol(source_image)
  target <- neuroim2::read_vol(target_image)
  warped <- neuroim2::read_vol(warped_image)
  fixed_mask <- neuroim2::read_vol(target_mask)
  moved_mask <- neuroim2::read_vol(warped_mask)
  fixed_labels <- neuroim2::read_vol(target_labels)
  moved_labels <- neuroim2::read_vol(warped_labels)
  jacobian_vol <- neuroim2::read_vol(jacobian)

  files <- c(
    source_native = "01-source-native.png",
    target_native = "02-target-native.png",
    checkerboard = "03-target-warped-checkerboard.png",
    mask_edges = "04-mask-edge-overlay.png",
    label_edges = "05-label-edge-overlay.png",
    jacobian = "06-jacobian-minus-one.png"
  )
  out <- file.path(output_dir, unname(files))
  names(out) <- names(files)

  write_visual_png(out[["source_native"]], function() {
    neuroim2::plot_ortho(
      source,
      style = "report",
      title = "Before: source template MNI152NLin6Asym",
      caption = "Native-grid context only; this panel is not an alignment comparison."
    )
  })
  write_visual_png(out[["target_native"]], function() {
    neuroim2::plot_ortho(
      target,
      style = "report",
      title = "Before: fixed template MNI152NLin2009cAsym",
      caption = "Native-grid context only; the candidate comparison is on this target grid."
    )
  })
  write_visual_png(out[["checkerboard"]], function() {
    neuroim2::plot_checkerboard(
      target,
      warped,
      tile = 16L,
      title = "After: target / warped-source checkerboard",
      subtitle = "Alternating tiles should not reveal duplicated structural boundaries.",
      caption = "Candidate visual QA only; refer to the frozen numerical gates."
    )
  })
  write_visual_png(out[["mask_edges"]], function() {
    neuroim2::plot_edge_overlay(
      target,
      visual_edge_map(fixed_mask),
      visual_edge_map(moved_mask),
      fixed_color = "#00d5ff",
      moving_color = "#ff3b30",
      title = "After: brain-mask edge agreement",
      subtitle = "Cyan = target mask; red = warped source mask.",
      caption = "Coincident contours appear blended; separated contours expose residual mismatch."
    )
  })
  write_visual_png(out[["label_edges"]], function() {
    neuroim2::plot_edge_overlay(
      target,
      visual_edge_map(fixed_labels, labels = TRUE),
      visual_edge_map(moved_labels, labels = TRUE),
      fixed_color = "#00d5ff",
      moving_color = "#ff3b30",
      title = "After: HOCPA label-boundary agreement",
      subtitle = "Cyan = target labels; red = warped source labels (GenericLabel interpolation).",
      caption = "This supplements, but does not replace, per-label Dice and ID-integrity checks."
    )
  })
  jacobian_delta <- neuroim2::NeuroVol(
    array(neuroim2::as.array(jacobian_vol) - 1, dim = dim(jacobian_vol)),
    neuroim2::space(jacobian_vol)
  )
  write_visual_png(out[["jacobian"]], function() {
    neuroim2::plot_overlay(
      target,
      jacobian_delta,
      ov_cmap = "blue-red",
      ov_symmetric = TRUE,
      ov_thresh = 0.05,
      ov_alpha_mode = "ramp",
      title = "After: Jacobian determinant minus one",
      subtitle = "Blue = contraction; red = expansion; near-zero change is transparent.",
      caption = "Visual context only. Non-positive and non-finite Jacobians remain hard failures."
    )
  })

  manifest <- list(
    schema_version = 1,
    qualitative_only = TRUE,
    renderer = "neuroim2",
    inputs = list(
      source_image = source_image,
      target_image = target_image,
      warped_image = warped_image,
      target_mask = target_mask,
      warped_mask = warped_mask,
      target_labels = target_labels,
      warped_labels = warped_labels,
      jacobian = jacobian
    ),
    files = lapply(out, function(path) {
      list(
        path = basename(path),
        bytes = unname(file.info(path)$size),
        sha256 = sha256_file(path)
      )
    })
  )
  write_json(manifest, file.path(output_dir, "visual-qa.json"))
  html <- c(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Neuroatlas transform visual QA</title>",
    "<style>body{font-family:sans-serif;max-width:1200px;margin:auto}img{width:100%;border:1px solid #bbb;margin:12px 0}</style>",
    "</head><body><h1>Transform visual QA</h1>",
    "<p>These panels support human review. They do not replace frozen numerical, direction, landmark, or Jacobian gates.</p>"
  )
  for (name in names(files)) {
    html <- c(html, paste0("<h2>", gsub("_", " ", name), "</h2><img src=\"", files[[name]], "\" alt=\"", name, "\">"))
  }
  html <- c(html, "</body></html>")
  writeLines(html, file.path(output_dir, "index.html"))
  manifest
}

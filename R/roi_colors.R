# Shared utilities ---------------------------------------------------------

#' Wrap hue angles into [0, 360)
#' @keywords internal
#' @noRd
wrap_hue <- function(h) {
  ((h %% 360) + 360) %% 360
}

#' Convert hex colours to 0-1 RGB matrix
#' @keywords internal
#' @noRd
hex_to_rgb01 <- function(hex) {
  m <- grDevices::col2rgb(hex) / 255
  t(m)
}

#' Convert 0-1 RGB matrix back to hex
#' @keywords internal
#' @noRd
rgb01_to_hex <- function(rgb) {
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 1)
}

#' Blend two RGB colours using alpha compositing
#' @keywords internal
#' @noRd
blend_rgb <- function(fg_rgb, bg_rgb, alpha) {
  alpha * fg_rgb + (1 - alpha) * bg_rgb
}

#' WCAG contrast ratio between two colours
#' @keywords internal
#' @noRd
contrast_ratio_wcag <- function(hex1, hex2) {
  srgb_to_lin <- function(u) {
    ifelse(u <= 0.04045, u / 12.92, ((u + 0.055) / 1.055)^2.4)
  }

  rel_lum <- function(hex) {
    rgb <- hex_to_rgb01(hex)
    lin <- srgb_to_lin(rgb)
    0.2126 * lin[, 1] + 0.7152 * lin[, 2] + 0.0722 * lin[, 3]
  }

  L1 <- rel_lum(hex1)
  L2 <- rel_lum(hex2)
  hi <- pmax(L1, L2)
  lo <- pmin(L1, L2)
  (hi + 0.05) / (lo + 0.05)
}

#' Filter candidate colours so they stay legible over anatomy slices
#' @keywords internal
#' @noRd
filter_by_overlay_contrast <- function(cols,
                                       bg_hex = "#808080",
                                       alpha = 0.85,
                                       min_contrast = 1.8) {
  fg <- hex_to_rgb01(cols)
  bg <- hex_to_rgb01(rep(bg_hex, length(cols)))
  blended <- blend_rgb(fg, bg, alpha)
  blended_hex <- rgb01_to_hex(blended)
  keep <- contrast_ratio_wcag(blended_hex, rep(bg_hex, length(cols))) >= min_contrast
  cols[keep]
}

#' Convert hex strings into Lab coordinates
#' @keywords internal
#' @noRd
hex_to_lab <- function(hex) {
  rgb <- hex_to_rgb01(hex)
  xyz <- grDevices::convertColor(rgb, from = "sRGB", to = "XYZ", scale.in = 1)
  grDevices::convertColor(xyz, from = "XYZ", to = "Lab", scale.in = 1)
}

#' Euclidean distance matrix helper
#' @keywords internal
#' @noRd
euclid_dist_mat <- function(A, B) {
  An <- rowSums(A * A)
  Bn <- rowSums(B * B)
  D2 <- outer(An, Bn, "+") - 2 * (A %*% t(B))
  sqrt(pmax(D2, 0))
}

#' Adjust hemisphere colours slightly toward white/black
#' @keywords internal
#' @noRd
adjust_by_hemi <- function(hex, hemi, amount = 0.08) {
  if (is.null(hemi)) {
    return(hex)
  }
  rgb <- hex_to_rgb01(hex)
  out <- rgb

  isL <- hemi %in% c("L", "Left", "left")
  isR <- hemi %in% c("R", "Right", "right")

  out[isL, ] <- out[isL, ] + amount * (1 - out[isL, ])
  out[isR, ] <- out[isR, ] * (1 - amount)
  rgb01_to_hex(out)
}


# Conflict graph and optimisation helpers ----------------------------------

#' Build slice-aware conflict edges between ROIs
#'
#' Constructs a sparse, symmetric edge list describing which ROIs are likely
#' to collide visually across common slice views. The resulting tibble records
#' the ROI indices, names, and a normalised conflict weight in \[0, 1\].
#'
#' @param rois Tibble with ROI metadata. Must contain `id_col` and the three
#'   columns listed in `xyz_cols`.
#' @param id_col Column containing ROI IDs.
#' @param xyz_cols Character vector of length three giving the coordinates to
#'   use for distance calculations.
#' @param k Number of nearest neighbours to evaluate when forming edges.
#' @param sigma_3d Spatial decay parameter in millimetres. Defaults to the
#'   median neighbour distance.
#' @param views Character vector of anatomical views to consider. Supported:
#'   `"axial"`, `"coronal"`, and `"sagittal"`.
#' @param view_weights Named numeric vector of weights for the supplied views.
#' @param sigma_xy In-plane decay (mm) for slice visibility.
#' @param sigma_slice Through-slice decay (mm) for slice visibility.
#' @param hemi_col Optional hemisphere column to down-weight cross-hemisphere
#'   conflicts.
#' @param cross_hemi_factor Multiplicative factor (< 1) applied to conflicts
#'   involving opposite hemispheres.
#' @param network_col Optional network column to up-weight cross-network
#'   conflicts.
#' @param diff_network_factor Multiplicative factor (> 1) applied to conflicts
#'   involving different networks.
#' @param weight_transform Optional function `function(edges, rois)` that can
#'   modify the weights before returning.
#'
#' @return Tibble with columns `from_idx`, `to_idx`, `from`, `to`, and `w`.
#'
#' @examples
#' \donttest{
#' rois <- data.frame(
#'   roi = 1:6,
#'   x = c(10, 12, 50, 52, 10, 50),
#'   y = c(20, 22, 20, 22, 60, 60),
#'   z = c(30, 32, 30, 32, 30, 30)
#' )
#' edges <- build_conflict_edges(rois, xyz_cols = c("x", "y", "z"))
#' }
#'
#' @export
build_conflict_edges <- function(rois,
                                 id_col = "roi",
                                 xyz_cols = c("x", "y", "z"),
                                 k = 12,
                                 sigma_3d = NULL,
                                 views = c("axial", "coronal", "sagittal"),
                                 view_weights = c(axial = 1, coronal = 1, sagittal = 1),
                                 sigma_xy = 25,
                                 sigma_slice = 10,
                                 hemi_col = NULL,
                                 cross_hemi_factor = 0.85,
                                 network_col = NULL,
                                 diff_network_factor = 1.15,
                                 weight_transform = NULL) {
  stopifnot(is.data.frame(rois))
  stopifnot(all(c(id_col, xyz_cols) %in% names(rois)))

  ids <- rois[[id_col]]
  xyz <- as.matrix(rois[, xyz_cols])
  n <- nrow(rois)
  if (n < 2) {
    stop("Need at least two ROIs.")
  }
  k <- min(k, n - 1)

  if (requireNamespace("FNN", quietly = TRUE)) {
    nn <- FNN::get.knn(xyz, k = k)
    nn_idx <- nn$nn.index
    nn_dist <- nn$nn.dist
  } else {
    D <- as.matrix(stats::dist(xyz))
    diag(D) <- Inf
    nn_idx <- t(apply(D, 1, function(r) order(r)[seq_len(k)]))
    nn_dist <- t(apply(D, 1, function(r) sort(r)[seq_len(k)]))
  }

  from <- rep(seq_len(n), each = k)
  to <- as.vector(t(nn_idx))
  d3 <- as.vector(t(nn_dist))

  i <- pmin(from, to)
  j <- pmax(from, to)
  edges <- data.frame(i = i, j = j, d3 = d3)
  edges <- edges[!duplicated(edges[, c("i", "j")]), , drop = FALSE]

  if (is.null(sigma_3d)) {
    sigma_3d <- stats::median(edges$d3)
  }

  xi <- xyz[edges$i, 1]
  yi <- xyz[edges$i, 2]
  zi <- xyz[edges$i, 3]
  xj <- xyz[edges$j, 1]
  yj <- xyz[edges$j, 2]
  zj <- xyz[edges$j, 3]
  dx <- xi - xj
  dy <- yi - yj
  dz <- zi - zj

  w <- exp(-(edges$d3^2) / (2 * sigma_3d^2))

  add_view <- function(view) {
    if (view == "axial") {
      d_plane <- sqrt(dx^2 + dy^2)
      d_slice <- abs(dz)
    } else if (view == "coronal") {
      d_plane <- sqrt(dx^2 + dz^2)
      d_slice <- abs(dy)
    } else if (view == "sagittal") {
      d_plane <- sqrt(dy^2 + dz^2)
      d_slice <- abs(dx)
    } else {
      stop("Unknown view: ", view)
    }
    exp(-(d_plane^2) / (2 * sigma_xy^2)) * exp(-(d_slice^2) / (2 * sigma_slice^2))
  }

  for (v in views) {
    vw <- if (!is.null(names(view_weights)) && v %in% names(view_weights)) {
      view_weights[[v]]
    } else {
      1
    }
    w <- w + vw * add_view(v)
  }

  if (!is.null(hemi_col) && hemi_col %in% names(rois)) {
    hi <- rois[[hemi_col]][edges$i]
    hj <- rois[[hemi_col]][edges$j]
    diff_hemi <- !is.na(hi) & !is.na(hj) & hi != hj
    w[diff_hemi] <- w[diff_hemi] * cross_hemi_factor
  }

  if (!is.null(network_col) && network_col %in% names(rois)) {
    ni <- rois[[network_col]][edges$i]
    nj <- rois[[network_col]][edges$j]
    diff_net <- !is.na(ni) & !is.na(nj) & ni != nj
    w[diff_net] <- w[diff_net] * diff_network_factor
  }

  w <- w / max(w)

  out <- tibble::tibble(
    from_idx = edges$i,
    to_idx = edges$j,
    from = ids[edges$i],
    to = ids[edges$j],
    w = w
  )

  if (!is.null(weight_transform)) {
    out <- weight_transform(out, rois)
    if (!("w" %in% names(out))) {
      stop("weight_transform must return a tibble with column 'w'.")
    }
  }

  out
}

#' Greedy soft-min colour assignment
#' @keywords internal
#' @noRd
greedy_assign_colors <- function(rois,
                                 edges,
                                 id_col = "roi",
                                 candidates_hex,
                                 candidate_group = NULL,
                                 roi_group = NULL,
                                 tau = 10,
                                 lambda_global = 0,
                                 seed = 1) {
  stopifnot(is.data.frame(rois))
  n <- nrow(rois)
  ids <- rois[[id_col]]

  if (length(candidates_hex) < n) {
    stop("Need at least as many candidate colours as ROIs.")
  }

  cand_lab <- hex_to_lab(candidates_hex)
  M <- nrow(cand_lab)

  nbr <- vector("list", n)
  wts <- vector("list", n)
  for (r in seq_len(nrow(edges))) {
    i <- edges$from_idx[r]
    j <- edges$to_idx[r]
    w <- edges$w[r]
    nbr[[i]] <- c(nbr[[i]], j)
    wts[[i]] <- c(wts[[i]], w)
    nbr[[j]] <- c(nbr[[j]], i)
    wts[[j]] <- c(wts[[j]], w)
  }
  deg <- vapply(wts, function(x) if (length(x)) sum(x) else 0, numeric(1))
  order_idx <- order(deg, decreasing = TRUE)

  assigned_hex <- rep(NA_character_, n)
  assigned_lab <- matrix(NA_real_, nrow = n, ncol = 3)
  unused <- rep(TRUE, M)

  set.seed(seed)

  for (ii in order_idx) {
    allowed <- which(unused)
    if (!is.null(candidate_group) && !is.null(roi_group)) {
      g <- roi_group[ii]
      if (!is.na(g)) {
        allowed_g <- allowed[candidate_group[allowed] == g]
        if (length(allowed_g) > 0) {
          allowed <- allowed_g
        }
      }
    }
    if (length(allowed) == 0) {
      stop("Ran out of candidate colours.")
    }

    nn <- nbr[[ii]]
    if (length(nn) > 0) {
      mask <- !is.na(assigned_hex[nn])
      nn <- nn[mask]
      ww <- wts[[ii]][mask]
    } else {
      ww <- numeric(0)
    }

    if (length(nn) == 0) {
      used_idx <- which(!is.na(assigned_hex))
      if (length(used_idx) == 0) {
        pick <- sample(allowed, 1)
      } else {
        D <- euclid_dist_mat(
          cand_lab[allowed, , drop = FALSE],
          assigned_lab[used_idx, , drop = FALSE]
        )
        minD <- apply(D, 1, min)
        pick <- allowed[which.max(minD)]
      }
    } else {
      neigh_lab <- assigned_lab[nn, , drop = FALSE]
      D <- euclid_dist_mat(cand_lab[allowed, , drop = FALSE], neigh_lab)
      w_norm <- ww / sum(ww)
      S <- exp(-D / tau) %*% matrix(w_norm, ncol = 1)
      score <- -log(pmax(S[, 1], 1e-12))

      if (lambda_global > 0) {
        used_idx <- which(!is.na(assigned_hex))
        if (length(used_idx) > 0) {
          Dg <- euclid_dist_mat(
            cand_lab[allowed, , drop = FALSE],
            assigned_lab[used_idx, , drop = FALSE]
          )
          minDg <- apply(Dg, 1, min)
          score <- score + lambda_global * minDg
        }
      }
      pick <- allowed[which.max(score)]
    }

    assigned_hex[ii] <- candidates_hex[pick]
    assigned_lab[ii, ] <- cand_lab[pick, ]
    unused[pick] <- FALSE
  }

  tibble::tibble(!!id_col := ids, color = assigned_hex)
}

#' Generate well-spread candidate colours using HCL sampling
#' @keywords internal
#' @noRd
make_candidate_pool <- function(n,
                                oversample = 12,
                                L_range = c(45, 80),
                                C_range = c(35, 90),
                                bg_hex = "#808080",
                                alpha = 0.85,
                                min_contrast = 1.8,
                                seed = 1) {
  set.seed(seed)
  N0 <- max(n * oversample, n + 200)

  H <- stats::runif(N0, 0, 360)
  C <- stats::runif(N0, C_range[1], C_range[2])
  L <- stats::runif(N0, L_range[1], L_range[2])

  cols <- grDevices::hcl(h = H, c = C, l = L, fixup = TRUE)
  cols <- unique(cols)
  cols <- filter_by_overlay_contrast(
    cols,
    bg_hex = bg_hex,
    alpha = alpha,
    min_contrast = min_contrast
  )
  cols <- unique(cols)
  if (length(cols) < n) {
    stop("Candidate filtering too strict; relax min_contrast or widen ranges.")
  }

  lab <- hex_to_lab(cols)
  picked <- integer(0)
  picked[1] <- sample(seq_len(nrow(lab)), 1)
  dmin <- rep(Inf, nrow(lab))
  for (k in seq_len(n - 1) + 1) {
    last <- picked[k - 1]
    d <- sqrt(rowSums((lab - matrix(lab[last, ], nrow(lab), 3, byrow = TRUE))^2))
    dmin <- pmin(dmin, d)
    picked[k] <- which.max(dmin)
  }
  cols[picked]
}


# Network-aware helpers ----------------------------------------------------

#' Assign harmonic anchor hues to networks
#'
#' @param network_levels Character vector of unique network names.
#' @param scheme Hue spacing scheme: `"even"`, `"triadic"`, `"tetradic"`, or
#'   `"complementary"`.
#' @param start_hue Starting hue in degrees.
#'
#' @return Named numeric vector of anchor hues.
#'
#' @examples
#' hues <- network_anchor_hues(c("Visual", "Default", "DorsalAttn"))
#' hues
#'
#' @export
network_anchor_hues <- function(network_levels,
                                scheme = c("even", "triadic", "tetradic", "complementary"),
                                start_hue = 15) {
  scheme <- match.arg(scheme)
  K <- length(network_levels)

  if (scheme == "complementary" && K == 2) {
    step <- 180
  } else if (scheme == "triadic" && K == 3) {
    step <- 120
  } else if (scheme == "tetradic" && K == 4) {
    step <- 90
  } else {
    step <- 360 / max(K, 1)
  }

  hues <- wrap_hue(start_hue + step * (seq_len(K) - 1))
  stats::setNames(hues, network_levels)
}

#' Build network-specific candidate palettes
#' @keywords internal
#' @noRd
make_network_candidates <- function(network_levels,
                                    n_per_network,
                                    anchor_hues,
                                    hue_width = 28,
                                    L_range = c(45, 80),
                                    C_range = c(35, 90),
                                    bg_hex = "#808080",
                                    alpha = 0.85,
                                    min_contrast = 1.8,
                                    seed = 1) {
  set.seed(seed)

  out_hex <- character(0)
  out_group <- character(0)

  for (net in network_levels) {
    M <- n_per_network[[net]]
    h0 <- anchor_hues[[net]]

    H <- stats::rnorm(M * 8, mean = h0, sd = hue_width / 2)
    H <- wrap_hue(H)
    C <- stats::runif(M * 8, C_range[1], C_range[2])
    L <- stats::runif(M * 8, L_range[1], L_range[2])

    cols <- grDevices::hcl(H, C, L, fixup = TRUE)
    cols <- unique(cols)
    cols <- filter_by_overlay_contrast(
      cols,
      bg_hex = bg_hex,
      alpha = alpha,
      min_contrast = min_contrast
    )
    cols <- unique(cols)

    if (length(cols) < M) {
      stop("Not enough candidates for network ", net, "; widen hue_width or relax filters.")
    }

    lab <- hex_to_lab(cols)
    picked <- integer(0)
    picked[1] <- sample(seq_len(nrow(lab)), 1)
    dmin <- rep(Inf, nrow(lab))
    for (k in seq_len(M - 1) + 1) {
      last <- picked[k - 1]
      d <- sqrt(rowSums((lab - matrix(lab[last, ], nrow(lab), 3, byrow = TRUE))^2))
      dmin <- pmin(dmin, d)
      picked[k] <- which.max(dmin)
    }
    cols <- cols[picked]

    out_hex <- c(out_hex, cols)
    out_group <- c(out_group, rep(net, length(cols)))
  }

  tibble::tibble(hex = out_hex, group = out_group)
}


# Public colour recipes ----------------------------------------------------

#' Slice-aware maximin palette for ROIs
#'
#' Assign perceptually separated colours by penalising conflicts between ROIs
#' that are likely to appear together in axial/coronal/sagittal views. This is a
#' strong default for general slice and surface visualisation.
#'
#' @inheritParams build_conflict_edges
#' @param rois Tibble with one ROI per row containing `roi`, `x`, `y`, and `z`.
#' @param hemi_col Optional hemisphere column used for the final light/dark
#'   adjustment.
#' @param network_col Optional network label column to slightly boost
#'   cross-network conflicts.
#' @param pair_col Optional column identifying homologous L/R ROI pairs. When
#'   supplied, colours are first assigned to the homolog pair and then nudged per
#'   hemisphere.
#' @param candidate_multiplier How many candidate colours to generate relative
#'   to the number of ROIs.
#' @param tau Soft-min temperature; smaller emphasises hard minimum distance,
#'   larger smooths the objective.
#' @param lambda_global Optional additional term that encourages global colour
#'   diversity.
#' @param bg_hex Background grey (for contrast filtering).
#' @param alpha Opacity of overlays when evaluating contrast.
#' @param seed Random seed for reproducibility.
#' @param weight_transform Optional weight adjustment callback passed to
#'   [build_conflict_edges()].
#'
#' @return Tibble with ROI IDs and assigned colours.
#'
#' @examples
#' \donttest{
#' rois <- data.frame(
#'   roi = 1:10,
#'   x = runif(10, 0, 90), y = runif(10, 0, 100), z = runif(10, 0, 80)
#' )
#' pal <- roi_colors_maximin_view(rois, xyz_cols = c("x", "y", "z"))
#' }
#'
#' @export
roi_colors_maximin_view <- function(rois,
                                    id_col = "roi",
                                    xyz_cols = c("x", "y", "z"),
                                    hemi_col = NULL,
                                    network_col = NULL,
                                    pair_col = NULL,
                                    k = 12,
                                    sigma_xy = 25,
                                    sigma_slice = 10,
                                    candidate_multiplier = 10,
                                    tau = 10,
                                    lambda_global = 0.15,
                                    bg_hex = "#808080",
                                    alpha = 0.85,
                                    seed = 1,
                                    weight_transform = NULL) {
  stopifnot(all(c(id_col, xyz_cols) %in% names(rois)))
  rois0 <- rois

  use_pairs <- !is.null(pair_col) && pair_col %in% names(rois0)
  if (use_pairs) {
    base_id <- ifelse(
      !is.na(rois0[[pair_col]]),
      rois0[[pair_col]],
      rois0[[id_col]]
    )
    rois_base <- rois0
    rois_base$.base_id <- base_id
    rois_work <- dplyr::group_by(rois_base, .data$.base_id)
    rois_work <- dplyr::summarise(
      rois_work,
      !!id_col := dplyr::first(.data$.base_id),
      dplyr::across(
        dplyr::all_of(xyz_cols),
        ~ mean(.x, na.rm = TRUE)
      ),
      hemi = if (!is.null(hemi_col) && hemi_col %in% names(rois0)) {
        dplyr::first(.data[[hemi_col]])
      } else {
        NA_character_
      },
      network = if (!is.null(network_col) && network_col %in% names(rois0)) {
        dplyr::first(.data[[network_col]])
      } else {
        NA_character_
      },
      .groups = "drop"
    )
    hemi_work <- NULL
    network_work <- if (!is.null(network_col)) {
      "network"
    } else {
      NULL
    }
  } else {
    rois_work <- rois0
    hemi_work <- hemi_col
    network_work <- network_col
  }

  edges <- build_conflict_edges(
    rois_work,
    id_col = id_col,
    xyz_cols = xyz_cols,
    k = k,
    sigma_xy = sigma_xy,
    sigma_slice = sigma_slice,
    hemi_col = hemi_work,
    network_col = network_work,
    weight_transform = weight_transform
  )

  N <- nrow(rois_work)
  n_cand <- max(N * candidate_multiplier, N + 200)
  candidates <- make_candidate_pool(
    n = n_cand,
    bg_hex = bg_hex,
    alpha = alpha,
    seed = seed
  )

  pal_base <- greedy_assign_colors(
    rois = rois_work,
    edges = edges,
    id_col = id_col,
    candidates_hex = candidates,
    tau = tau,
    lambda_global = lambda_global,
    seed = seed
  )

  if (use_pairs) {
    pal_work <- pal_base
    pal_work$.base_id <- pal_work[[id_col]]
    pal_work <- pal_work[, c(".base_id", "color")]

    pal <- rois0
    pal$.base_id <- base_id
    pal <- dplyr::left_join(pal, pal_work, by = ".base_id")
    if (!is.null(hemi_col) && hemi_col %in% names(rois0)) {
      pal$color <- adjust_by_hemi(pal$color, pal[[hemi_col]])
    }
    pal <- dplyr::select(pal, dplyr::all_of(c(id_col, "color")))
  } else {
    pal <- pal_base
    if (!is.null(hemi_col) && hemi_col %in% names(rois0)) {
      pal <- dplyr::left_join(
        rois0[, c(id_col, hemi_col)],
        pal,
        by = id_col
      )
      pal$color <- adjust_by_hemi(pal$color, pal[[hemi_col]])
      pal <- dplyr::select(pal, dplyr::all_of(c(id_col, "color")))
    }
  }

  pal
}

#' Network-harmonic palette with neighbour separation
#'
#' Assign colours so that networks share analogous hue families while still
#' maximising spatial separation between neighbouring ROIs.
#'
#' @inheritParams roi_colors_maximin_view
#' @param scheme Hue spacing scheme passed to [network_anchor_hues()].
#' @param start_hue Starting hue in degrees (0--360).
#' @param hue_width Half-width (degrees) of the analogous band around each
#'   anchor hue.
#'
#' @return Tibble with ROI IDs and hex colours.
#'
#' @examples
#' \donttest{
#' rois <- data.frame(
#'   roi = 1:10, network = rep(c("Vis", "DMN"), 5),
#'   x = runif(10, 0, 90), y = runif(10, 0, 100), z = runif(10, 0, 80)
#' )
#' pal <- roi_colors_network_harmony(rois, xyz_cols = c("x","y","z"),
#'                                    network_col = "network")
#' }
#'
#' @export
roi_colors_network_harmony <- function(rois,
                                       id_col = "roi",
                                       xyz_cols = c("x", "y", "z"),
                                       network_col = "network",
                                       hemi_col = NULL,
                                       k = 12,
                                       sigma_xy = 25,
                                       sigma_slice = 10,
                                       scheme = "even",
                                       start_hue = 15,
                                       hue_width = 28,
                                       candidate_multiplier = 8,
                                       tau = 10,
                                       lambda_global = 0.1,
                                       bg_hex = "#808080",
                                       alpha = 0.85,
                                       seed = 1,
                                       weight_transform = NULL) {
  stopifnot(all(c(id_col, xyz_cols, network_col) %in% names(rois)))

  edges <- build_conflict_edges(
    rois,
    id_col = id_col,
    xyz_cols = xyz_cols,
    k = k,
    sigma_xy = sigma_xy,
    sigma_slice = sigma_slice,
    hemi_col = hemi_col,
    network_col = network_col,
    weight_transform = weight_transform
  )

  nets <- sort(unique(as.character(rois[[network_col]])))
  nets <- nets[!is.na(nets)]
  if (length(nets) < 1) {
    stop("network_col has no non-NA values.")
  }
  anchors <- network_anchor_hues(nets, scheme = scheme, start_hue = start_hue)

  n_by_net <- table(as.character(rois[[network_col]]))
  n_by_net <- n_by_net[nets]
  n_cand <- as.list(ceiling(as.numeric(n_by_net) * candidate_multiplier))
  names(n_cand) <- nets

  cand_tbl <- make_network_candidates(
    network_levels = nets,
    n_per_network = n_cand,
    anchor_hues = anchors,
    hue_width = hue_width,
    bg_hex = bg_hex,
    alpha = alpha,
    seed = seed
  )

  roi_group <- as.character(rois[[network_col]])
  pal <- greedy_assign_colors(
    rois = rois,
    edges = edges,
    id_col = id_col,
    candidates_hex = cand_tbl$hex,
    candidate_group = cand_tbl$group,
    roi_group = roi_group,
    tau = tau,
    lambda_global = lambda_global,
    seed = seed
  )

  if (!is.null(hemi_col) && hemi_col %in% names(rois)) {
    pal <- dplyr::left_join(
      rois[, c(id_col, hemi_col)],
      pal,
      by = id_col
    )
    pal$color <- adjust_by_hemi(pal$color, pal[[hemi_col]])
    pal <- dplyr::select(pal, dplyr::all_of(c(id_col, "color")))
  }

  pal
}

#' Deterministic HCL palette with harmonic variation
#'
#' Generates a fast, reproducible palette that combines harmonic network hues
#' with anterior-posterior gradients and hemisphere luminance differences.
#'
#' @inheritParams roi_colors_network_harmony
#' @param hue_width Half-width (degrees) of within-network hue modulation.
#' @param C Fixed chroma.
#' @param L_L Lightness for left hemisphere ROIs.
#' @param L_R Lightness for right hemisphere ROIs.
#'
#' @return Tibble with ROI IDs and colours.
#'
#' @examples
#' \donttest{
#' rois <- data.frame(
#'   roi = 1:10, network = rep(c("Vis", "DMN"), 5),
#'   hemi = rep(c("left", "right"), 5),
#'   x = runif(10), y = runif(10), z = runif(10)
#' )
#' pal <- roi_colors_rule_hcl(rois, network_col = "network",
#'                             hemi_col = "hemi", xyz_cols = c("x","y","z"))
#' }
#'
#' @export
roi_colors_rule_hcl <- function(rois,
                                id_col = "roi",
                                xyz_cols = c("x", "y", "z"),
                                network_col = NULL,
                                hemi_col = NULL,
                                scheme = "even",
                                start_hue = 15,
                                hue_width = 30,
                                C = 70,
                                L_L = 72,
                                L_R = 60) {
  stopifnot(all(c(id_col, xyz_cols) %in% names(rois)))

  n <- nrow(rois)
  if (n < 1) {
    stop("No ROIs supplied.")
  }

  if (!is.null(network_col) && network_col %in% names(rois)) {
    nets <- sort(unique(as.character(rois[[network_col]])))
    nets <- nets[!is.na(nets)]
    anchors <- network_anchor_hues(nets, scheme = scheme, start_hue = start_hue)
    net <- as.character(rois[[network_col]])
    anchor_h <- anchors[net]
    anchor_h[is.na(anchor_h)] <- start_hue
  } else {
    anchor_h <- rep(start_hue, n)
  }

  coord_y <- rois[[xyz_cols[2]]]
  if (all(is.na(coord_y))) {
    coord_y <- seq_len(n)
  }
  r <- rank(coord_y, ties.method = "average", na.last = "keep")
  if (any(is.na(r))) {
    r[is.na(r)] <- mean(r, na.rm = TRUE)
  }
  tval <- (r - min(r)) / (max(r) - min(r) + 1e-9)
  hue_offset <- (tval * 2 - 1) * hue_width

  h <- wrap_hue(anchor_h + hue_offset)

  if (!is.null(hemi_col) && hemi_col %in% names(rois)) {
    hemi <- rois[[hemi_col]]
    L <- ifelse(
      hemi %in% c("L", "Left", "left"),
      L_L,
      ifelse(hemi %in% c("R", "Right", "right"), L_R, (L_L + L_R) / 2)
    )
  } else {
    L <- rep((L_L + L_R) / 2, n)
  }

  cols <- grDevices::hcl(h = h, c = C, l = L, fixup = TRUE)
  tibble::tibble(!!id_col := rois[[id_col]], color = cols)
}

#' Embedding-based palette with smooth gradients
#'
#' Project ROI features to two dimensions (UMAP or PCA) and map polar angle to
#' hue, yielding palettes that look globally structured.
#'
#' @param feature_cols Character vector of numeric or categorical columns that
#'   will be embedded (passed to `stats::model.matrix()`).
#' @param method Either `"umap"` (requires the `uwot` package) or `"pca"`.
#' @param C_range Chroma range mapped from radial distance in the embedding.
#' @param L Lightness value used for all ROIs before hemispheric adjustments.
#'
#' @inheritParams roi_colors_maximin_view
#'
#' @return Tibble with ROI IDs and colours.
#'
#' @examples
#' \donttest{
#' rois <- data.frame(
#'   roi = 1:10, network = rep(c("Vis", "DMN"), 5),
#'   x = runif(10), y = runif(10), z = runif(10)
#' )
#' pal <- roi_colors_embedding(rois, feature_cols = c("x","y","z"),
#'                              method = "pca")
#' }
#'
#' @export
roi_colors_embedding <- function(rois,
                                 id_col = "roi",
                                 feature_cols = c("x", "y", "z"),
                                 hemi_col = NULL,
                                 method = c("umap", "pca"),
                                 C_range = c(40, 90),
                                 L = 65,
                                 seed = 1) {
  method <- match.arg(method)
  stopifnot(all(c(id_col, feature_cols) %in% names(rois)))

  df <- as.data.frame(rois[, feature_cols, drop = FALSE])
  X <- stats::model.matrix(~ 0 + ., data = df)
  X <- scale(X)

  set.seed(seed)
  if (method == "umap" && requireNamespace("uwot", quietly = TRUE)) {
    E <- uwot::umap(
      X,
      n_components = 2,
      n_neighbors = min(15L, nrow(X) - 1L),
      min_dist = 0.25,
      metric = "euclidean",
      ret_model = FALSE
    )
  } else {
    pc <- stats::prcomp(X, rank. = 2)
    E <- pc$x[, 1:2, drop = FALSE]
  }

  e1 <- E[, 1]
  e2 <- E[, 2]
  angle <- atan2(e2, e1)
  hue <- wrap_hue(angle * 180 / pi)

  r <- sqrt(e1^2 + e2^2)
  r <- (r - min(r)) / (max(r) - min(r) + 1e-9)
  C <- C_range[1] + r * (C_range[2] - C_range[1])

  cols <- grDevices::hcl(h = hue, c = C, l = L, fixup = TRUE)
  if (!is.null(hemi_col) && hemi_col %in% names(rois)) {
    cols <- adjust_by_hemi(cols, rois[[hemi_col]], amount = 0.07)
  }

  tibble::tibble(!!id_col := rois[[id_col]], color = cols)
}

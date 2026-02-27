#' Compute Parcel Adjacency Graph from an Atlas
#'
#' @description
#' Builds a region adjacency graph from a volumetric atlas. Two parcels are
#' considered adjacent when at least one pair of their voxels are neighbours
#' under the chosen connectivity scheme.
#'
#' @param atlas An atlas object (must contain \code{$atlas}, \code{$ids}, and
#'   \code{$labels}).
#' @param connectivity Character. Neighbourhood type: \code{"6"} (face),
#'   \code{"18"} (face + edge), or \code{"26"} (face + edge + corner).
#'   Default: \code{"6"}.
#' @param as Character. Output format: \code{"matrix"} (K x K adjacency
#'   matrix), \code{"igraph"} (\pkg{igraph} graph object), or \code{"tibble"}
#'   (edge-list data frame with \code{from}, \code{to}, \code{weight} columns).
#'   Default: \code{"matrix"}.
#' @param include_weight Logical. If \code{TRUE} (default), edge values are the
#'   count of shared boundary voxel pairs. If \code{FALSE}, edges are binary
#'   (0/1).
#'
#' @return Depending on \code{as}:
#' \describe{
#'   \item{matrix}{A symmetric integer matrix of dimension K x K with row/column
#'     names set to region labels.}
#'   \item{igraph}{An undirected \pkg{igraph} graph (requires the \pkg{igraph}
#'     package).}
#'   \item{tibble}{A \code{data.frame} (tibble if available) with columns
#'     \code{from}, \code{to}, and \code{weight}.}
#' }
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(100)
#' adj   <- atlas_graph(atlas)
#' adj_b <- atlas_graph(atlas, include_weight = FALSE)
#' el    <- atlas_graph(atlas, as = "tibble")
#' }
#'
#' @export
atlas_graph <- function(atlas,
                        connectivity = c("6", "18", "26"),
                        as = c("matrix", "igraph", "tibble"),
                        include_weight = TRUE) {

  connectivity <- match.arg(connectivity)
  as <- match.arg(as)

  # --- 1. Densify atlas volume to a plain integer array ---
  vol <- atlas$atlas
  if (methods::is(vol, "ClusteredNeuroVol")) {
    arr <- array(0L, dim = dim(vol))
    arr[which(vol@mask)] <- as.integer(vol@clusters)
  } else {
    arr <- array(as.integer(vol[, , ]), dim = dim(vol))
  }

  dims <- dim(arr)

  # --- 2. Build positive-half neighbour offsets ---
  offsets <- matrix(c(1L, 0L, 0L,
                      0L, 1L, 0L,
                      0L, 0L, 1L), ncol = 3, byrow = TRUE)

  if (connectivity %in% c("18", "26")) {
    edge_offsets <- matrix(c(
      1L, 1L, 0L,
      1L, -1L, 0L,
      1L, 0L, 1L,
      1L, 0L, -1L,
      0L, 1L, 1L,
      0L, 1L, -1L
    ), ncol = 3, byrow = TRUE)
    offsets <- rbind(offsets, edge_offsets)
  }

  if (connectivity == "26") {
    corner_offsets <- matrix(c(
      1L, 1L, 1L,
      1L, 1L, -1L,
      1L, -1L, 1L,
      1L, -1L, -1L
    ), ncol = 3, byrow = TRUE)
    offsets <- rbind(offsets, corner_offsets)
  }

  # --- 3. Identify unique region IDs and set up mapping ---
  unique_ids <- sort(unique(arr[arr != 0L]))
  K <- length(unique_ids)

  if (K == 0L) {
    stop("Atlas volume contains no labelled voxels.")
  }

  adj <- matrix(0L, K, K)

  # --- 4. For each offset, find boundary pairs (vectorised) ---
  for (oi in seq_len(nrow(offsets))) {
    dx <- offsets[oi, 1L]
    dy <- offsets[oi, 2L]
    dz <- offsets[oi, 3L]

    # Valid index ranges for the source and shifted slices
    x1 <- seq.int(max(1L, 1L - dx), min(dims[1], dims[1] - dx))
    y1 <- seq.int(max(1L, 1L - dy), min(dims[1], dims[2] - dy))
    z1 <- seq.int(max(1L, 1L - dz), min(dims[1], dims[3] - dz))

    x2 <- x1 + dx
    y2 <- y1 + dy
    z2 <- z1 + dz

    a <- arr[x1, y1, z1]
    b <- arr[x2, y2, z2]

    mask <- a != 0L & b != 0L & a != b
    if (!any(mask)) next

    ia <- match(a[mask], unique_ids)
    ib <- match(b[mask], unique_ids)

    edge_key <- (ia - 1L) * K + ib
    edge_counts <- tabulate(edge_key, nbins = K * K)
    adj <- adj + matrix(edge_counts, K, K)
  }

  # Symmetrise (we only used positive-half offsets)
  adj <- adj + t(adj)

  # --- 5. Label rows/columns ---
  id_to_label <- stats::setNames(atlas$labels, as.character(atlas$ids))
  region_labels <- unname(id_to_label[as.character(unique_ids)])
  dimnames(adj) <- list(region_labels, region_labels)

  # --- 6. Convert to requested output format ---
  if (!include_weight) {
    adj[adj > 0L] <- 1L
  }

  switch(as,
    matrix = adj,
    igraph = {
      if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("Package 'igraph' is required for as = \"igraph\".")
      }
      wt <- if (include_weight) TRUE else NULL
      igraph::graph_from_adjacency_matrix(adj, mode = "undirected",
                                          weighted = wt)
    },
    tibble = {
      # Upper-triangle edge list (avoid duplicate edges)
      idx <- which(adj > 0L & upper.tri(adj), arr.ind = TRUE)
      if (nrow(idx) == 0L) {
        el <- data.frame(from = character(0), to = character(0),
                         weight = integer(0), stringsAsFactors = FALSE)
      } else {
        el <- data.frame(
          from   = region_labels[idx[, 1L]],
          to     = region_labels[idx[, 2L]],
          weight = adj[idx],
          stringsAsFactors = FALSE
        )
      }
      if (requireNamespace("tibble", quietly = TRUE)) {
        tibble::as_tibble(el)
      } else {
        el
      }
    }
  )
}

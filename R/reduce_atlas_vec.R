#' @rdname reduce_atlas_vec
#'
#' @param stat_func Function used to aggregate voxel values within each parcel
#'   (default: \code{mean}).
#' @param dilate Logical. If \code{TRUE}, atlas parcels are dilated into the
#'   mask before averaging (via \code{\link{dilate_atlas}}).
#' @param radius Numeric dilation radius in voxels (passed to
#'   \code{\link{dilate_atlas}}). Default: 4.
#' @param maxn Integer maximum neighbours for dilation (passed to
#'   \code{\link{dilate_atlas}}). Default: 50.
#'
#' @details
#' The atlas volume is intersected with the brain mask so that only voxels
#' inside the mask contribute to each parcel average.
#'
#' Cluster IDs are remapped to contiguous \code{1:K} before constructing the
#' \code{ClusteredNeuroVec}. This is required because the \code{[,,,t]}
#' accessor in \pkg{neuroim2} uses cluster IDs as direct column indices into
#' the internal time-series matrix.
#'
#' Parcels that have no voxels inside the mask receive \code{NA} time-series
#' in the output.
#'
#' @examples
#' \dontrun{
#' atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
#' # data_vol: a NeuroVec with matching spatial dimensions
#' # mask:     a brain mask NeuroVol
#' cvec <- reduce_atlas_vec(atlas, data_vol, mask)
#' ts_mat <- as.matrix(cvec)   # T x K cluster time-series
#' }
#'
#' @importFrom neuroim2 ClusteredNeuroVec ClusteredNeuroVol LogicalNeuroVol
#'   NeuroSpace space spacing
#' @importFrom assertthat assert_that
#' @export
#' @method reduce_atlas_vec atlas
reduce_atlas_vec.atlas <- function(atlas, data_vol, mask,
                                   stat_func = mean,
                                   dilate = FALSE,
                                   radius = 4, maxn = 50, ...) {

  # --- Input validation -------------------------------------------------------
  assert_that(inherits(atlas, "atlas"),
              msg = "'atlas' must be an atlas object")
  assert_that(methods::is(data_vol, "NeuroVec"),
              msg = "'data_vol' must be a NeuroVec (4D) object")
  assert_that(methods::is(mask, "NeuroVol") || methods::is(mask, "LogicalNeuroVol"),
              msg = "'mask' must be a NeuroVol or LogicalNeuroVol")

  atlas_dims <- dim(.get_atlas_volume(atlas))[1:3]
  data_dims  <- dim(data_vol)[1:3]
  mask_dims  <- dim(mask)[1:3]

  assert_that(all(atlas_dims == data_dims),
              msg = sprintf("Spatial dimensions of atlas (%s) and data_vol (%s) must match",
                            paste(atlas_dims, collapse = "x"),
                            paste(data_dims, collapse = "x")))
  assert_that(all(atlas_dims == mask_dims),
              msg = sprintf("Spatial dimensions of atlas (%s) and mask (%s) must match",
                            paste(atlas_dims, collapse = "x"),
                            paste(mask_dims, collapse = "x")))

  # --- Coerce mask to logical --------------------------------------------------
  if (!methods::is(mask, "LogicalNeuroVol")) {
    mask <- neuroim2::LogicalNeuroVol(as.array(mask) != 0,
                                      space = neuroim2::space(mask))
  }

  # --- Optional dilation -------------------------------------------------------
  working_atlas <- atlas
  if (isTRUE(dilate)) {
    working_atlas <- dilate_atlas(atlas, mask, radius = radius, maxn = maxn)
  }

  # --- Densify atlas volume ----------------------------------------------------
  atlas_vol <- .get_atlas_volume(working_atlas)
  if (methods::is(atlas_vol, "ClusteredNeuroVol")) {
    atlas_arr <- array(0L, dim = dim(atlas_vol))
    mask_idx  <- which(atlas_vol@mask)
    atlas_arr[mask_idx] <- as.integer(atlas_vol@clusters)
  } else {
    atlas_arr <- as.integer(as.array(atlas_vol))
    dim(atlas_arr) <- dim(atlas_vol)
  }

  # --- Apply brain mask -------------------------------------------------------
  mask_logical <- as.logical(as.array(mask))
  dim(mask_logical) <- dim(mask)
  atlas_arr[!mask_logical] <- 0L

  # --- Identify present / missing clusters ------------------------------------
  orig_ids       <- atlas$ids
  present_ids    <- sort(unique(atlas_arr[atlas_arr != 0L]))
  missing_ids    <- setdiff(orig_ids, present_ids)
  all_ids_sorted <- sort(orig_ids)
  K              <- length(all_ids_sorted)

  if (K == 0L) {
    stop("Atlas has no region IDs")
  }

  # --- Remap cluster IDs to contiguous 1:K ------------------------------------
  # Build lookup:  original_id  ->  column index (1..K)
  id_to_col <- stats::setNames(seq_along(all_ids_sorted), as.character(all_ids_sorted))

  # Remap the masked atlas array
  remapped_arr <- array(0L, dim = dim(atlas_arr))
  nz <- which(atlas_arr != 0L)
  remapped_arr[nz] <- as.integer(id_to_col[as.character(atlas_arr[nz])])

  # --- Build a full (unmasked, remapped) atlas for the output -----------------
  # We need a ClusteredNeuroVol whose clusters span all K regions, so that the

  # output ClusteredNeuroVec has columns for every original atlas region.
  # Start from the *undilated, original* atlas so that the output covers all
  # parcels (including those that may not overlap the mask).
  full_atlas_vol <- .get_atlas_volume(atlas)
  if (methods::is(full_atlas_vol, "ClusteredNeuroVol")) {
    full_arr <- array(0L, dim = dim(full_atlas_vol))
    full_mask_idx <- which(full_atlas_vol@mask)
    full_arr[full_mask_idx] <- as.integer(full_atlas_vol@clusters)
  } else {
    full_arr <- as.integer(as.array(full_atlas_vol))
    dim(full_arr) <- dim(full_atlas_vol)
  }

  full_remapped <- array(0L, dim = dim(full_arr))
  full_nz <- which(full_arr != 0L)
  full_remapped[full_nz] <- as.integer(id_to_col[as.character(full_arr[full_nz])])

  sp3 <- neuroim2::space(full_atlas_vol)

  full_mask_vol <- neuroim2::LogicalNeuroVol(full_remapped != 0L, space = sp3)
  full_cvol <- neuroim2::ClusteredNeuroVol(
    mask     = full_mask_vol,
    clusters = as.integer(full_remapped[full_remapped != 0L]),
    label_map = stats::setNames(
      as.list(seq_len(K)),
      if (!is.null(atlas$labels)) atlas$labels[match(all_ids_sorted, orig_ids)]
      else as.character(all_ids_sorted)
    )
  )

  # --- Compute per-parcel time-series -----------------------------------------
  T_len <- dim(data_vol)[4]

  if (length(present_ids) > 0L) {
    # Build a masked ClusteredNeuroVol (only present clusters) for aggregation
    masked_mask_vol <- neuroim2::LogicalNeuroVol(remapped_arr != 0L, space = sp3)
    masked_cvol <- neuroim2::ClusteredNeuroVol(
      mask     = masked_mask_vol,
      clusters = as.integer(remapped_arr[remapped_arr != 0L])
    )

    # Let ClusteredNeuroVec aggregate the NeuroVec for present clusters
    partial <- neuroim2::ClusteredNeuroVec(x = data_vol, cvol = masked_cvol,
                                           FUN = stat_func)
    partial_ts <- partial@ts  # T x n_present matrix
  }

  # --- Assemble full T x K matrix with NA for missing clusters ----------------
  if (length(missing_ids) > 0L) {
    full_ts <- matrix(NA_real_, nrow = T_len, ncol = K)

    if (length(present_ids) > 0L) {
      present_cols <- as.integer(id_to_col[as.character(present_ids)])
      full_ts[, present_cols] <- partial_ts
    }

    result <- neuroim2::ClusteredNeuroVec(x = full_ts, cvol = full_cvol)
  } else {
    # All clusters present — use partial result directly but attach full cvol
    result <- neuroim2::ClusteredNeuroVec(x = partial_ts, cvol = full_cvol)
  }

  result
}

make_toy_cluster_explorer_inputs <- function(n_time = 4) {
  dims <- c(5, 5, 5)
  sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1),
                              origin = c(0, 0, 0))

  atlas_arr <- array(0L, dim = dims)
  atlas_arr[1:2, 1:2, 1:2] <- 1L
  atlas_arr[4:5, 4:5, 4:5] <- 2L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = sp3)

  atlas <- list(
    name = "toy_atlas",
    atlas = atlas_vol,
    ids = c(1L, 2L),
    labels = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi = c(NA, NA),
    roi_metadata = tibble::tibble(
      id = c(1L, 2L),
      label = c("A", "B"),
      hemi = c(NA_character_, NA_character_)
    )
  )
  class(atlas) <- c("toy", "atlas")

  stat_arr <- array(0, dim = dims)
  stat_arr[1:2, 1:2, 1:2] <- 4.5
  stat_arr[4:5, 4:5, 4:5] <- -5.5
  stat_arr[3, 3, 3] <- 9.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = sp3)

  sp4 <- neuroim2::NeuroSpace(dim = c(dims, n_time), spacing = c(1, 1, 1))
  data_arr <- array(0, dim = c(dims, n_time))
  for (t in seq_len(n_time)) {
    data_arr[1:2, 1:2, 1:2, t] <- t
    data_arr[4:5, 4:5, 4:5, t] <- 10 + t
  }
  data_vec <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    condition = rep(c("A", "B"), length.out = n_time),
    trial = seq_len(n_time)
  )

  list(
    atlas = atlas,
    stat_map = stat_map,
    data_vec = data_vec,
    sample_table = sample_table
  )
}

make_toy_surfatlas <- function() {
  surfatlas <- list(
    ids = c(1L, 2L),
    labels = c("A", "B"),
    hemi = c("left", "right")
  )
  class(surfatlas) <- c("toy_surf", "surfatlas", "atlas")
  surfatlas
}

make_toy_mismatch_cluster_explorer_inputs <- function(n_time = 4) {
  atlas_dims <- c(5, 5, 5)
  stat_dims <- c(6, 6, 6)

  atlas_space <- neuroim2::NeuroSpace(
    dim = atlas_dims,
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0)
  )
  stat_space <- neuroim2::NeuroSpace(
    dim = stat_dims,
    spacing = c(1, 1, 1),
    origin = c(0, 0, 0)
  )

  atlas_arr <- array(0L, dim = atlas_dims)
  atlas_arr[2:4, 2:4, 2:4] <- 1L
  atlas_vol <- neuroim2::NeuroVol(atlas_arr, space = atlas_space)
  atlas <- list(
    name = "toy_mismatch_atlas",
    atlas = atlas_vol,
    ids = 1L,
    labels = "A",
    orig_labels = "A",
    hemi = NA_character_,
    roi_metadata = tibble::tibble(
      id = 1L,
      label = "A",
      hemi = NA_character_
    )
  )
  class(atlas) <- c("toy", "atlas")

  stat_arr <- array(0, dim = stat_dims)
  stat_arr[2:4, 2:4, 2:4] <- 4.5
  stat_map <- neuroim2::NeuroVol(stat_arr, space = stat_space)

  sp4 <- neuroim2::NeuroSpace(dim = c(stat_dims, n_time), spacing = c(1, 1, 1))
  data_arr <- array(0, dim = c(stat_dims, n_time))
  for (t in seq_len(n_time)) {
    data_arr[2:4, 2:4, 2:4, t] <- t
  }
  data_vec <- neuroim2::NeuroVec(data_arr, sp4)

  sample_table <- tibble::tibble(
    condition = rep("A", n_time),
    trial = seq_len(n_time)
  )

  list(
    atlas = atlas,
    stat_map = stat_map,
    data_vec = data_vec,
    sample_table = sample_table
  )
}

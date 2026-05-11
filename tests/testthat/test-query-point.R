skip_on_cran()

# Helper to make a toy atlas for query_point tests
make_toy_atlas_qp <- function() {
  # 10x10x10 volume with spacing=2, origin=0
  # Grid (1,1,1) -> world (0,0,0)
  # Grid (i,j,k) -> world ((i-1)*2, (j-1)*2, (k-1)*2)
  arr <- array(0L, dim = c(10, 10, 10))
  arr[2:4, 2:4, 2:4] <- 1L
  arr[6:8, 6:8, 6:8] <- 2L
  arr[2:4, 6:8, 2:4] <- 3L

  sp <- neuroim2::NeuroSpace(dim = c(10, 10, 10),
                              spacing = c(2, 2, 2),
                              origin = c(0, 0, 0))
  vol <- neuroim2::NeuroVol(arr, sp)

  structure(list(
    name = "toy",
    atlas = vol,
    ids = c(1L, 2L, 3L),
    labels = c("RegionA", "RegionB", "RegionC"),
    orig_labels = c("RegionA", "RegionB", "RegionC"),
    hemi = c("left", "right", "left"),
    network = c("NetA", "NetB", "NetA")
  ), class = c("toy", "atlas"))
}

make_toy_clustered_atlas_qp <- function() {
  atlas <- make_toy_atlas_qp()
  arr <- as.integer(atlas$atlas[, , ])
  dim(arr) <- dim(atlas$atlas)
  label_map <- as.list(atlas$ids)
  names(label_map) <- atlas$labels

  atlas$atlas <- neuroim2::ClusteredNeuroVol(
    as.logical(atlas$atlas),
    clusters = arr[arr != 0L],
    label_map = label_map
  )
  atlas$name <- "toy_clustered"
  class(atlas) <- c("toy_clustered", "atlas")
  atlas
}

brute_radius_ids_qp <- function(atlas, coord, radius) {
  vol <- neuroatlas:::.get_atlas_volume(atlas)
  arr <- as.integer(vol[, , ])
  dim(arr) <- dim(vol)
  nz <- which(arr != 0L, arr.ind = TRUE)
  world <- neuroim2::grid_to_coord(neuroim2::space(vol), nz)
  dists <- sqrt(rowSums(
    (world - matrix(coord, nrow(world), 3L, byrow = TRUE))^2
  ))
  ids <- unique(arr[nz][dists <= radius + sqrt(.Machine$double.eps)])
  ids[ids != 0L]
}

test_that("single point lookup returns correct region", {
  atlas <- make_toy_atlas_qp()
  # Grid (3,3,3) -> world (4,4,4) -> region 1
  res <- query_point(c(4, 4, 4), atlas)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(res$id, 1L)
  expect_equal(res$label, "RegionA")
  expect_equal(res$hemi, "left")
  expect_equal(res$network, "NetA")
  expect_equal(res$atlas_name, "toy")

  # Grid (7,7,7) -> world (12,12,12) -> region 2
  res2 <- query_point(c(12, 12, 12), atlas)
  expect_equal(res2$id, 2L)
  expect_equal(res2$label, "RegionB")
  expect_equal(res2$hemi, "right")
  expect_equal(res2$network, "NetB")
})

test_that("atlas-first query wrappers return labels for coords and voxels", {
  atlas <- make_toy_atlas_qp()

  by_coord <- query_coord(atlas, c(4, 4, 4))
  expect_equal(by_coord$id, 1L)
  expect_equal(by_coord$label, "RegionA")

  by_vox <- query_vox(atlas, c(3, 3, 3))
  expect_equal(by_vox$id, 1L)
  expect_equal(by_vox$label, "RegionA")
  expect_equal(by_vox$x, 4)
  expect_equal(by_vox$y, 4)
  expect_equal(by_vox$z, 4)

  multi_vox <- query_vox(atlas, rbind(c(3, 3, 3), c(7, 7, 7)))
  expect_equal(multi_vox$id, c(1L, 2L))
  expect_equal(multi_vox$label, c("RegionA", "RegionB"))
})

test_that("query_vox supports atlas lists using first atlas grid", {
  atlas1 <- make_toy_atlas_qp()
  atlas2 <- make_toy_atlas_qp()
  atlas2$name <- "toy2"
  atlas2$labels <- c("R1", "R2", "R3")

  res <- query_vox(list(first = atlas1, second = atlas2), c(3, 3, 3))
  expect_equal(nrow(res), 2L)
  expect_equal(res$atlas_name, c("first", "second"))
  expect_equal(res$label, c("RegionA", "R1"))
})

test_that("exact lookup agrees for dense and clustered atlas volumes", {
  dense_atlas <- make_toy_atlas_qp()
  clustered_atlas <- make_toy_clustered_atlas_qp()
  pts <- rbind(
    c(4, 4, 4),
    c(12, 12, 12),
    c(0, 0, 0),
    c(100, 100, 100)
  )

  dense_res <- query_coord(dense_atlas, pts)
  clustered_res <- query_coord(clustered_atlas, pts)

  expect_equal(clustered_res$id, dense_res$id)
  expect_equal(clustered_res$label, dense_res$label)
  expect_equal(clustered_res$hemi, dense_res$hemi)
  expect_equal(clustered_res$network, dense_res$network)
  expect_equal(clustered_res$point, seq_len(nrow(pts)))
})

test_that("multi-point lookup returns correct tibble", {
  atlas <- make_toy_atlas_qp()
  pts <- rbind(
    c(4, 4, 4),    # region 1
    c(12, 12, 12)  # region 2
  )
  res <- query_point(pts, atlas)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L)
  expect_equal(res$id, c(1L, 2L))
  expect_equal(res$label, c("RegionA", "RegionB"))
  expect_equal(res$x, c(4, 12))
  expect_equal(res$y, c(4, 12))
  expect_equal(res$z, c(4, 12))
})

test_that("out-of-bounds coordinate returns NA", {
  atlas <- make_toy_atlas_qp()
  # World (100, 100, 100) -> way outside the 10x10x10 grid
  res <- query_point(c(100, 100, 100), atlas)
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$id))
  expect_true(is.na(res$label))
  expect_true(is.na(res$hemi))
})

test_that("background (id=0) coordinate returns NA", {
  atlas <- make_toy_atlas_qp()
  # Grid (1,1,1) -> world (0,0,0) -> background (arr[1,1,1] == 0)
  res <- query_point(c(0, 0, 0), atlas)
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$id))
  expect_true(is.na(res$label))
})

test_that("multi-atlas list returns results from all atlases", {
  atlas1 <- make_toy_atlas_qp()
  atlas2 <- make_toy_atlas_qp()
  atlas2$name <- "toy2"
  atlas2$labels <- c("R1", "R2", "R3")

  res <- query_point(c(4, 4, 4), list(first = atlas1, second = atlas2))
  expect_equal(nrow(res), 2L)
  expect_equal(res$atlas_name, c("first", "second"))
  expect_equal(res$label, c("RegionA", "R1"))
})

test_that("radius > 0 returns fuzzy results", {
  atlas <- make_toy_atlas_qp()
  # Query at a point between region 1 and region 3
  # Region 1: grid (2:4,2:4,2:4)
  # Region 3: grid (2:4,6:8,2:4)
  # Grid (3,5,3) -> world (4,8,4) is background between them
  # Region 1 nearest voxel: grid (3,4,3) -> world (4,6,4), dist = 2mm
  # Region 3 nearest voxel: grid (3,6,3) -> world (4,10,4), dist = 2mm
  res <- query_point(c(4, 8, 4), atlas, radius = 3)
  expect_s3_class(res, "tbl_df")
  # Should find at least region 1 and region 3
  expect_true(all(c(1L, 3L) %in% res$id))
})

test_that("radius lookup matches brute-force voxel-distance oracle", {
  atlas <- make_toy_atlas_qp()
  pts <- rbind(
    c(4, 8, 4),
    c(12, 12, 12),
    c(0, 0, 0)
  )
  radius <- 3

  res <- query_coord(atlas, pts, radius = radius)

  for (i in seq_len(nrow(pts))) {
    expected <- brute_radius_ids_qp(atlas, pts[i, ], radius)
    observed <- res$id[res$point == i]
    observed <- observed[!is.na(observed)]
    expect_equal(sort(observed), sort(expected))
  }
})

test_that("radius lookup agrees for dense and clustered atlas volumes", {
  dense_atlas <- make_toy_atlas_qp()
  clustered_atlas <- make_toy_clustered_atlas_qp()
  pts <- rbind(
    c(4, 8, 4),
    c(12, 12, 12),
    c(100, 100, 100)
  )

  dense_res <- query_coord(dense_atlas, pts, radius = 3)
  clustered_res <- query_coord(clustered_atlas, pts, radius = 3)

  expect_equal(clustered_res$id, dense_res$id)
  expect_equal(clustered_res$label, dense_res$label)
  expect_equal(clustered_res$hemi, dense_res$hemi)
  expect_equal(clustered_res$network, dense_res$network)
  expect_equal(clustered_res$point, dense_res$point)
  expect_true(all(dense_res$point[dense_res$id %in% c(1L, 3L)] == 1L))
  expect_true(any(is.na(dense_res$id) & dense_res$point == 3L))
})

test_that("query_point validates inputs", {
  atlas <- make_toy_atlas_qp()
  expect_error(query_point(c(1, 2), atlas), "length-3")
  expect_error(query_point(matrix(1:4, ncol = 2), atlas), "3 columns")
  expect_error(query_point(c(1, 2, 3), atlas, radius = -1), "non-negative")
  expect_error(query_vox(atlas, c(1, 2)), "length-3")
  expect_error(query_vox(atlas, matrix(1:4, ncol = 2)), "3 columns")
})

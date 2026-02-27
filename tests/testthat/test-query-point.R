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

test_that("query_point validates inputs", {
  atlas <- make_toy_atlas_qp()
  expect_error(query_point(c(1, 2), atlas), "length-3")
  expect_error(query_point(matrix(1:4, ncol = 2), atlas), "3 columns")
})

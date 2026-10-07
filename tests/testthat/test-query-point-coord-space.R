# Toy atlas on a 2 mm grid with origin 0: grid (i, j, k) -> world
# ((i - 1) * 2, (j - 1) * 2, (k - 1) * 2). Region `ids[1]` fills grid
# [2:3, 2:3, 2:3] (world 2-4 mm); region `ids[2]` fills [6:7, 6:7, 6:7].
make_space_atlas_qp <- function(name, ids, coord_space) {
  sp <- neuroim2::NeuroSpace(
    c(10L, 10L, 10L),
    spacing = c(2, 2, 2),
    origin = c(0, 0, 0)
  )
  arr <- array(0L, c(10, 10, 10))
  arr[2:3, 2:3, 2:3] <- ids[1]
  arr[6:7, 6:7, 6:7] <- ids[2]
  mask <- neuroim2::LogicalNeuroVol(arr != 0, sp)
  x <- list(
    name = name,
    atlas = neuroim2::ClusteredNeuroVol(mask, clusters = arr[arr != 0]),
    cmap = data.frame(r = c(10, 20), g = c(30, 40), b = c(50, 60)),
    ids = ids,
    labels = paste0(name, "_", ids),
    orig_labels = paste0(name, "_", ids),
    hemi = c("left", "right")
  )
  class(x) <- c(name, "atlas")
  .attach_atlas_ref(x, new_atlas_ref(name, name, coord_space = coord_space))
}

test_that(
  "a merge of atlases in different spaces can be queried",
  {
    a <- make_space_atlas_qp("A", c(1L, 2L), "MNI152")
    # B's regions sit elsewhere so that both parents keep their voxels.
    b <- make_space_atlas_qp("B", c(1L, 2L), "MNI305")
    b_arr <- array(0L, c(10, 10, 10))
    b_arr[2:3, 7:8, 2:3] <- 1L
    b_arr[7:8, 2:3, 7:8] <- 2L
    b$atlas <- neuroim2::ClusteredNeuroVol(
      neuroim2::LogicalNeuroVol(b_arr != 0, neuroim2::space(a$atlas)),
      clusters = b_arr[b_arr != 0]
    )

    merged <- merge_atlases(a, b)
    expect_true(is.na(atlas_coord_space(merged)))
    expect_true(is.na(merged$atlas_ref$coord_space))

    # Voxel centres: A_1 at grid (2,2,2), B_1 at (2,7,2), B_2 at (7,2,7).
    pts <- rbind(c(2, 2, 2), c(2, 12, 2), c(12, 2, 12), c(16, 16, 16))
    expect_warning(
      hit <- query_point(pts, merged),
      class = "neuroatlas_unknown_coord_space"
    )
    expect_identical(hit$label, c("A_1", "B_1", "B_2", NA))
    # Coordinates are reported unchanged (no transform was applied).
    expect_equal(
      as.matrix(hit[, c("x", "y", "z")]),
      pts,
      ignore_attr = TRUE
    )

    # The warning names the atlas and the requested input space.
    msg <- tryCatch(
      query_point(pts[1, ], merged, from_space = "MNI305"),
      warning = conditionMessage
    )
    expect_match(msg, "A::B", fixed = TRUE)
    expect_match(msg, "no transform from 'MNI305'", fixed = TRUE)

    # Radius search works on the composite too.
    expect_warning(
      near <- query_point(c(2, 12, 2), merged, radius = 2),
      class = "neuroatlas_unknown_coord_space"
    )
    expect_true("B_1" %in% near$label)
  }
)

test_that(
  "NA, empty and 'Unknown' coord_space skip the transform with a warning",
  {
    for (cs in c(NA_character_, "", "Unknown", "unknown")) {
      atl <- make_space_atlas_qp("T", c(5L, 9L), cs)
      expect_warning(
        hit <- query_point(rbind(c(2, 2, 2), c(12, 12, 12)), atl),
        class = "neuroatlas_unknown_coord_space"
      )
      expect_identical(hit$id, c(5L, 9L), info = cs)
    }
  }
)

test_that(
  "annotated atlases keep their transform behaviour",
  {
    same <- make_space_atlas_qp("S", c(1L, 2L), "MNI152")
    expect_no_warning(hit <- query_point(c(2, 2, 2), same))
    expect_identical(hit$id, 1L)

    # MNI305 atlas queried with MNI152 input: the transformed coordinate is
    # looked up, matching an explicit transform_coords() + MNI305 query.
    other <- make_space_atlas_qp("O", c(1L, 2L), "MNI305")
    pt <- c(12.4, 12.1, 11.6)
    expect_no_warning(via_transform <- query_point(pt, other))
    explicit <- query_point(
      transform_coords(pt, "MNI152", "MNI305"),
      other,
      from_space = "MNI305"
    )
    expect_identical(via_transform$id, explicit$id)
    expect_equal(c(via_transform$x, via_transform$y, via_transform$z), pt)

    # A declared space without a transform remains an error.
    scanner <- make_space_atlas_qp("X", c(1L, 2L), "Scanner")
    expect_error(
      suppressWarnings(query_point(c(2, 2, 2), scanner)),
      "No transform available"
    )
  }
)

test_that(
  "only the unannotated atlas in a list warns",
  {
    annotated <- make_space_atlas_qp("S", c(1L, 2L), "MNI152")
    unknown <- make_space_atlas_qp("U", c(3L, 4L), NA_character_)
    warnings <- character(0)
    hit <- withCallingHandlers(
      query_point(c(2, 2, 2), list(s = annotated, u = unknown)),
      neuroatlas_unknown_coord_space = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, 1L)
    expect_match(warnings, "Atlas 'u'", fixed = TRUE)
    expect_identical(hit$id, c(1L, 3L))
  }
)

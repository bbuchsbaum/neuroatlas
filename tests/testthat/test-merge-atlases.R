context("merge_atlases")

test_that(
  "merge_atlases combines ids and preserves dimensions",
  {
    skip_on_cran()
    a1 <- get_aseg_atlas()
    a2 <- get_aseg_atlas()
    merged <- merge_atlases(a1, a2)

    expect_true(inherits(merged, "atlas"))
    expect_equal(length(merged$ids), length(a1$ids) + length(a2$ids))
    expect_equal(dim(merged$atlas), dim(a1$atlas))
    expect_s3_class(merged$cmap, "data.frame")
    expect_equal(nrow(merged$cmap), length(merged$ids))
  }
)

test_that(
  "merge_atlases keeps Schaefer networks when merged with ASEG",
  {
    skip_on_cran()
    aseg <- get_aseg_atlas()
    schaefer <- tryCatch(
      get_schaefer_atlas("100", "7", outspace = neuroim2::space(aseg$atlas)),
      error = function(e) {
        skip(
          paste(
            "Schaefer atlas unavailable:",
            conditionMessage(e)
          )
        )
      }
    )

    for (order in c("schaefer_first", "aseg_first")) {
      merged <- if (order == "schaefer_first") {
        merge_atlases(schaefer, aseg)
      } else {
        merge_atlases(aseg, schaefer)
      }
      n_s <- length(schaefer$ids)
      n_a <- length(aseg$ids)
      s_rows <- if (order == "schaefer_first") seq_len(n_s) else n_a + seq_len(n_s)
      a_rows <- setdiff(seq_along(merged$ids), s_rows)

      expect_length(merged$network, n_s + n_a)
      expect_identical(merged$network[s_rows], as.character(schaefer$network))
      expect_true(all(is.na(merged$network[a_rows])))

      meta <- roi_metadata(merged)
      expect_true("network" %in% names(meta), info = order)
      expect_identical(meta$network[s_rows], as.character(schaefer$network))
      expect_true("network" %in% roi_attributes(merged))

      # A cortical Schaefer voxel reports its network through query_point().
      s_vol <- methods::as(schaefer$atlas, "array")
      target <- schaefer$ids[which(schaefer$network == "Default")[1]]
      ijk <- which(s_vol == target, arr.ind = TRUE)[1, , drop = FALSE]
      xyz <- neuroim2::grid_to_coord(neuroim2::space(schaefer$atlas), ijk)
      hit <- query_point(xyz, merged)
      expect_identical(hit$network, "Default", info = order)
      expect_identical(hit$label, schaefer$labels[schaefer$ids == target])
    }
  }
)

test_that(
  "merge_atlases carries other per-region attributes and fills NA",
  {
    sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
    make <- function(name, ids, region, extra = list()) {
      arr <- array(0L, c(4, 4, 4))
      arr[region] <- ids[1]
      arr[region + 32L] <- ids[2]
      mask <- neuroim2::LogicalNeuroVol(arr != 0, sp)
      x <- c(
        list(
          name = name,
          atlas = neuroim2::ClusteredNeuroVol(mask, clusters = arr[arr != 0]),
          cmap = data.frame(r = c(10, 20), g = c(30, 40), b = c(50, 60)),
          ids = ids,
          labels = paste0(name, "_", ids),
          orig_labels = paste0(name, "_", ids),
          hemi = c("left", "right")
        ),
        extra
      )
      class(x) <- c(name, "atlas")
      x
    }
    a <- make(
      "A",
      c(1L, 2L),
      1:2,
      list(
        network = factor(c("Vis", "Default")),
        lobe = c("occipital", "frontal")
      )
    )
    b <- make("B", c(1L, 2L), 3:4, list(tissue = c("gm", "wm")))

    merged <- merge_atlases(a, b)
    expect_identical(merged$network, c("Vis", "Default", NA, NA))
    expect_identical(merged$lobe, c("occipital", "frontal", NA, NA))
    expect_identical(merged$tissue, c(NA, NA, "gm", "wm"))
    meta <- roi_metadata(merged)
    expect_identical(meta$id, c(1L, 2L, 3L, 4L))
    expect_identical(meta$network, c("Vis", "Default", NA, NA))
    expect_identical(meta$tissue, c(NA, NA, "gm", "wm"))

    # Two atlases without networks gain no network column.
    plain <- merge_atlases(make("C", c(1L, 2L), 1:2), make("D", c(1L, 2L), 3:4))
    expect_null(plain$network)
    expect_false("network" %in% names(roi_metadata(plain)))
  }
)

test_that(
  "merge_atlases remaps each atlas2 voxel exactly once",
  {
    sp <- neuroim2::NeuroSpace(c(4L, 4L, 4L), spacing = c(2, 2, 2))
    make <- function(name, ids, arr) {
      mask <- neuroim2::LogicalNeuroVol(arr != 0, sp)
      x <- list(
        name = name,
        atlas = neuroim2::ClusteredNeuroVol(mask, clusters = arr[arr != 0]),
        cmap = data.frame(r = seq_along(ids), g = 0, b = 0),
        ids = ids,
        labels = paste0(name, ids),
        orig_labels = paste0(name, ids),
        hemi = rep(NA_character_, length(ids))
      )
      class(x) <- c(name, "atlas")
      x
    }
    a1 <- array(0L, c(4, 4, 4))
    a1[1, 1, 1] <- 1L
    a1[2, 1, 1] <- 2L
    # atlas2 ids 1..4 become 3..6; 1 -> 3 must not be remapped again by 3 -> 5.
    a2 <- array(0L, c(4, 4, 4))
    a2[1:4, 4, 4] <- 1:4
    # Declared id 6 has no voxels; it must still get a distinct new id.
    merged <- merge_atlases(make("A", 1:2, a1), make("B", c(1:4, 6L), a2))

    expect_identical(merged$ids, c(1L, 2L, 3L, 4L, 5L, 6L, 7L))
    vol <- methods::as(merged$atlas, "array")
    expect_identical(as.integer(vol[1:4, 4, 4]), 3:6)
    expect_identical(as.integer(vol[1:2, 1, 1]), 1:2)
    expect_identical(
      merged$labels[match(vol[1:4, 4, 4], merged$ids)],
      paste0("B", 1:4)
    )
  }
)

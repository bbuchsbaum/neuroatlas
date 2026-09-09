test_that(".merge_ggseg_mapped_data preserves geometry-like columns", {
  ggseg_data <- tibble::tibble(
    label = c("lh_a", "rh_b"),
    region = c("A", "B"),
    hemi = c("left", "right"),
    geometry = list("geom_lh", "geom_rh")
  )

  mapped_data <- tibble::tibble(
    label = c("lh_a", "rh_b"),
    region = c("A", "B"),
    hemi = c("left", "right"),
    statistic = c(1.5, -2.0)
  )

  merged <- neuroatlas:::.merge_ggseg_mapped_data(ggseg_data, mapped_data)

  expect_equal(merged$geometry, ggseg_data$geometry)
  expect_equal(merged$statistic, mapped_data$statistic)
  expect_equal(nrow(merged), nrow(ggseg_data))
})

test_that(".merge_ggseg_mapped_data overwrites stale non-key columns", {
  ggseg_data <- tibble::tibble(
    label = c("a", "b"),
    statistic = c(99, 99),
    geometry = list("g1", "g2")
  )

  mapped_data <- tibble::tibble(
    label = c("a", "b"),
    statistic = c(1, 2)
  )

  merged <- neuroatlas:::.merge_ggseg_mapped_data(ggseg_data, mapped_data)

  expect_equal(merged$statistic, c(1, 2))
  expect_equal(merged$geometry, ggseg_data$geometry)
})

test_that(".merge_ggseg_mapped_data errors when there are no shared keys", {
  ggseg_data <- tibble::tibble(
    parcel = c("a", "b"),
    geometry = list("g1", "g2")
  )

  mapped_data <- tibble::tibble(
    atlas_label = c("a", "b"),
    statistic = c(1, 2)
  )

  expect_error(
    neuroatlas:::.merge_ggseg_mapped_data(ggseg_data, mapped_data),
    "no shared key columns"
  )
})

test_that(".merge_ggseg_mapped_data preserves sf geometry class", {
  skip_if_not_installed("sf")

  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))),
    sf::st_polygon(list(rbind(
      c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)
    )))
  )

  ggseg_data <- sf::st_sf(
    label = c("lh_a", "rh_b"),
    region = c("A", "B"),
    hemi = c("left", "right"),
    geometry = geom
  )

  mapped_data <- tibble::tibble(
    label = c("lh_a", "rh_b"),
    region = c("A", "B"),
    hemi = c("left", "right"),
    statistic = c(1.5, -2.0)
  )

  merged <- neuroatlas:::.merge_ggseg_mapped_data(ggseg_data, mapped_data)

  expect_s3_class(merged, "sf")
  expect_equal(
    sf::st_as_text(sf::st_geometry(merged)),
    sf::st_as_text(sf::st_geometry(ggseg_data))
  )
  expect_equal(merged$statistic, mapped_data$statistic)
})

test_that(".load_ggseg_schaefer_atlas uses namespace object when available", {
  ns <- new.env(parent = emptyenv())
  ns$schaefer17_400 <- list(name = "from_namespace")

  loader <- function(list, package, envir) {
    stop("data_loader should not be called when namespace lookup succeeds")
  }

  out <- neuroatlas:::.load_ggseg_schaefer_atlas(
    "schaefer17_400",
    namespace_env = ns,
    data_loader = loader
  )

  expect_identical(out, ns$schaefer17_400)
})

test_that(".load_ggseg_schaefer_atlas resolves callable atlas constructors", {
  ns <- new.env(parent = emptyenv())
  ns$schaefer17_400 <- function() list(name = "from_callable", data = list())

  out <- neuroatlas:::.load_ggseg_schaefer_atlas(
    "schaefer17_400",
    namespace_env = ns,
    data_loader = function(...) stop("unused")
  )

  expect_equal(out$name, "from_callable")
  expect_false(is.function(out))
})

test_that(".ggseg_atlas_xy_data flattens nested ggseg geom polygons", {
  gg_atlas <- list(
    data = list(
      geom = tibble::tibble(
        label = c("lh_A", "rh_B"),
        geometry = list(
          tibble::tibble(
            view = c("lateral", "medial"),
            x = c(0, 1),
            y = c(2, 3)
          ),
          tibble::tibble(
            view = c("lateral", "medial"),
            x = c(4, 5),
            y = c(6, 7)
          )
        )
      )
    )
  )

  out <- neuroatlas:::.ggseg_atlas_xy_data(gg_atlas)
  expect_equal(nrow(out), 4L)
  expect_setequal(out$hemi, c("left", "right"))
  expect_setequal(out$view, c("lateral", "medial"))
  expect_true(all(c("x", "y", "hemi", "view", "label") %in% names(out)))
})

test_that(".ggseg_atlas_xy_data supports legacy flat side/hemi tables", {
  gg_atlas <- list(
    data = data.frame(
      hemi = c("left", "right"),
      side = c("lateral", "medial"),
      x = c(1, 2),
      y = c(3, 4),
      stringsAsFactors = FALSE
    )
  )

  out <- neuroatlas:::.ggseg_atlas_xy_data(gg_atlas)
  expect_equal(out$view, c("lateral", "medial"))
  expect_equal(out$hemi, c("left", "right"))
})

test_that(".load_ggseg_schaefer_atlas falls back to data()", {
  ns <- new.env(parent = emptyenv())

  loader <- function(list, package, envir) {
    assign(list, list(name = "from_data"), envir = envir)
    invisible(character(0))
  }

  out <- neuroatlas:::.load_ggseg_schaefer_atlas(
    "schaefer17_400",
    namespace_env = ns,
    data_loader = loader
  )

  expect_equal(out$name, "from_data")
})

test_that(".load_ggseg_schaefer_atlas errors when both paths fail", {
  ns <- new.env(parent = emptyenv())

  loader <- function(list, package, envir) {
    stop("simulated data() failure")
  }

  expect_error(
    neuroatlas:::.load_ggseg_schaefer_atlas(
      "schaefer17_400",
      namespace_env = ns,
      data_loader = loader
    ),
    "Failed to load atlas 'schaefer17_400'"
  )
})

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

# Regression tests for plot.atlas() against the neuroim2 >= 0.19 plotting
# redesign, where plot_ortho() returns one assembled patchwork figure by
# default instead of a list of per-plane ggplots.

test_that(".atlas_value_ids maps numeric, factor, character and list values", {
  expect_identical(.atlas_value_ids(c(0, 1.2, 2.8, NA)),
                   c("0", "1", "3", NA))
  expect_identical(.atlas_value_ids(c(0L, 17L)), c("0", "17"))
  expect_identical(.atlas_value_ids(factor(c("0", "17", "53"))),
                   c("0", "17", "53"))
  expect_identical(.atlas_value_ids(c("4", "10.0", "bad")),
                   c("4", "10", NA))
  expect_identical(.atlas_value_ids(list(2, 5, NULL)), c("2", "5", NA))
})

test_that(".call_neuroim2_plot drops optional args the function lacks", {
  f_old <- function(vol, zlevels = NULL, ...) list(zlevels = zlevels,
                                                   extra = list(...))
  f_new <- function(vol, assemble = TRUE, interpolate = TRUE) {
    list(assemble = assemble, interpolate = interpolate)
  }
  old <- .call_neuroim2_plot(f_old, 1, args = list(zlevels = 3),
                             optional = list(assemble = FALSE))
  expect_identical(old$zlevels, 3)
  expect_length(old$extra, 0)

  new <- .call_neuroim2_plot(f_new, 1,
                             optional = list(assemble = FALSE,
                                             interpolate = FALSE),
                             dots = list(interpolate = TRUE))
  expect_false(new$assemble)
  expect_true(new$interpolate)  # user-supplied value wins
})

test_that("plot(get_aseg_atlas(), view = 'ortho') builds and renders", {
  atlas <- tryCatch(get_aseg_atlas(), error = function(e) NULL)
  skip_if(is.null(atlas), "aseg atlas unavailable")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- plot(atlas, view = "ortho")

  panels <- if (inherits(p, "patchwork")) lapply(seq_along(p), function(i) p[[i]]) else p
  expect_length(panels, 3)
  for (pl in panels) {
    expect_s3_class(pl$data$value, "factor")
    # Every non-background voxel maps onto an atlas region id.
    ids <- as.character(pl$data$value)
    ids <- ids[!is.na(ids) & ids != "0"]
    expect_gt(length(ids), 0)
    expect_true(all(ids %in% as.character(atlas$ids)))
    expect_no_error(ggplot2::ggplot_build(pl))
  }
})

test_that("plot(get_aseg_atlas()) montage builds with region colours", {
  atlas <- tryCatch(get_aseg_atlas(), error = function(e) NULL)
  skip_if(is.null(atlas), "aseg atlas unavailable")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- plot(atlas, nslices = 4)
  expect_true(inherits(p, "ggplot"))
  expect_s3_class(p$data$value, "factor")
})

anterior_side <- function(hemi, view) {
  orient <- neuroatlas:::.surface_orientation_annotations(hemi, view)
  a <- orient[orient$label == "A", ]
  p <- orient[orient$label == "P", ]
  if (a$x != p$x) {
    if (a$x < p$x) "left" else "right"
  } else {
    if (a$y > p$y) "top" else "bottom"
  }
}

test_that(
  "anterior label follows the CPU camera for every view",
  {
    expected <- c(
      "left.lateral" = "left",
      "left.medial" = "right",
      "right.lateral" = "right",
      "right.medial" = "left",
      "left.dorsal" = "top",
      "right.dorsal" = "top",
      "left.ventral" = "bottom",
      "right.ventral" = "bottom"
    )
    for (key in names(expected)) {
      parts <- strsplit(key, ".", fixed = TRUE)[[1]]
      expect_identical(
        anterior_side(parts[1], parts[2]),
        expected[[key]],
        info = key
      )
    }
    expect_identical(anterior_side("lh", "medial"), "right")
    expect_identical(anterior_side("rh", "medial"), "left")
  }
)

test_that(
  "anterior label agrees with neurosurf's camera basis",
  {
    skip_if_not_installed("neurosurf")
    basis_fn <- tryCatch(
      utils::getFromNamespace(".ns_camera_basis", "neurosurf"),
      error = function(e) NULL
    )
    skip_if(is.null(basis_fn), "neurosurf has no .ns_camera_basis")
    for (hemi in c("left", "right")) {
      for (view in c("lateral", "medial", "dorsal", "ventral")) {
        for (obliquity in c(0, 7)) {
          basis <- basis_fn(view, if (hemi == "left") "lh" else "rh", obliquity)
          # Columns are screen-right, screen-up, toward; row 2 is anterior (+y).
          right_y <- basis[2, 1]
          up_y <- basis[2, 2]
          expected <- if (abs(right_y) > abs(up_y)) {
            if (right_y > 0) "right" else "left"
          } else {
            if (up_y > 0) "top" else "bottom"
          }
          expect_identical(
            anterior_side(hemi, view),
            expected,
            info = paste(hemi, view, obliquity)
          )
        }
      }
    }
  }
)

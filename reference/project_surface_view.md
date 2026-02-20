# Project Surface Vertices to a Canonical 2D View

Public wrapper for the core surface-view projection used by
[`plot_brain()`](plot_brain.md).

## Usage

``` r
project_surface_view(
  verts,
  view = c("lateral", "medial", "dorsal", "ventral"),
  hemi = c("left", "right")
)
```

## Arguments

- verts:

  Numeric matrix (\\N \times 3\\) of vertex coordinates.

- view:

  Character scalar: one of `"lateral"`, `"medial"`, `"dorsal"`,
  `"ventral"`.

- hemi:

  Character scalar: `"left"` or `"right"`.

## Value

A list with elements:

- `xy`:

  Numeric matrix (\\N \times 2\\) of projected coordinates.

- `view_dir`:

  Numeric length-3 view direction vector used for backface culling.

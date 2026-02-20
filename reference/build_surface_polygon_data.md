# Build Surface Polygon Data for Rendering

Build 2D projected polygon/boundary data from a `surfatlas` for custom
rendering workflows. This exposes the mesh-projection data pipeline used
internally by [`plot_brain()`](plot_brain.md).

## Usage

``` r
build_surface_polygon_data(
  surfatlas,
  views = c("lateral", "medial"),
  surface = "inflated",
  merged = TRUE,
  use_cache = TRUE
)
```

## Arguments

- surfatlas:

  A surface atlas object inheriting from class `"surfatlas"`.

- views:

  Character vector of views to include. Any combination of `"lateral"`,
  `"medial"`, `"dorsal"`, `"ventral"`.

- surface:

  Character scalar identifying the surface type label.

- merged:

  Logical. If `TRUE` (default), returns merged parcel-level polygons
  (faster, fewer shapes). If `FALSE`, returns per-triangle polygons.

- use_cache:

  Logical. If `TRUE` (default), use memoized builders.

## Value

A list with components:

- `polygons`:

  A tibble of projected polygon vertices.

- `boundaries`:

  A tibble of projected boundary segments.

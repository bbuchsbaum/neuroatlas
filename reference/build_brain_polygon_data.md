# Build brain polygon render data for a surface atlas

Projects surface atlas geometry into 2D polygon panels for each
hemisphere and view. Results are memoised so repeated calls with
identical arguments return cached data.

## Usage

``` r
build_brain_polygon_data(surfatlas, views, surface, projection_smooth = 0L)
```

## Arguments

- surfatlas:

  A surfatlas object (e.g. from [`schaefer_surf()`](schaefer_surf.md)).

- views:

  Character vector of views to render (e.g. `c("lateral", "medial")`).

- surface:

  Character: surface type (`"inflated"`, `"pial"`, `"white"`).

- projection_smooth:

  Integer: number of Laplacian smoothing iterations on projected
  coordinates (default `0L`).

## Value

A list with two elements:

- `polygons`:

  Tibble of 2D projected polygon vertices with columns
  `x, y, poly_id, parcel_id, label, hemi, view, panel`.

- `boundaries`:

  Tibble of boundary edges with columns `x, y, xend, yend, panel`.

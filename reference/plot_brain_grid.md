# Multi-panel Brain Plot Grid

Arranges multiple brain surface plots into a grid layout using
patchwork. Each element of `vals_list` produces one panel rendered by
[`plot_brain`](plot_brain.md).

## Usage

``` r
plot_brain_grid(
  surfatlas,
  vals_list = NULL,
  views = c("lateral", "medial"),
  hemis = c("left", "right"),
  ncol = NULL,
  shared_scale = TRUE,
  palette = "cork",
  lim = NULL,
  titles = NULL,
  colorbar = TRUE,
  colorbar_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  data = NULL,
  values = NULL,
  by = NULL,
  allow_partial = FALSE,
  ...
)
```

## Arguments

- surfatlas:

  A `surfatlas` object (e.g., from [`schaefer_surf`](schaefer_surf.md)
  or [`glasser_surf`](glasser_surf.md)).

- vals_list:

  A list of numeric vectors, one per panel. Each vector must have length
  equal to the number of atlas regions. Optional when `data` and
  `values` are supplied. Names, when present, become the default panel
  titles.

- views:

  Character vector of views passed to [`plot_brain`](plot_brain.md).
  Default: `c("lateral", "medial")`.

- hemis:

  Character vector of hemispheres passed to
  [`plot_brain`](plot_brain.md). Default: `c("left", "right")`.

- ncol:

  Integer number of columns in the grid layout. If `NULL`, chosen
  automatically.

- shared_scale:

  Logical. If `TRUE` (default), all panels share the same colour scale
  computed from the range of all values.

- palette:

  Character: scico palette name. Default: `"cork"`.

- lim:

  Optional numeric length-2 colour limits. Overrides automatic limits
  when provided.

- titles:

  Optional character vector of panel titles. If `NULL`, names of
  `vals_list` are used.

- colorbar:

  Logical or character. Use `TRUE` or `"right"` (default) for a vertical
  shared colorbar, `"bottom"` for a horizontal shared colorbar, or
  `FALSE` / `"none"` to omit it.

- colorbar_title:

  Optional shared colorbar title.

- title, subtitle, caption:

  Optional overall plot annotations applied to the composed figure.

- data:

  Optional data frame, tibble, or `parcel_data` object with one row per
  parcel. Supply either `data` or `vals_list`.

- values:

  Character vector naming numeric columns in `data`; one brain map is
  produced for each column.

- by:

  Parcel-key specification passed to
  [`align_parcel_values()`](align_parcel_values.md).

- allow_partial:

  Logical. If `FALSE` (default), `data` must contain every atlas parcel.
  If `TRUE`, missing parcels receive `NA` values.

- ...:

  Additional arguments passed to [`plot_brain`](plot_brain.md).

## Value

A `patchwork` object.

## Examples

``` r
if (FALSE) { # \dontrun{
atl <- schaefer_surf(200, 7)
vals <- list(
  Contrast_A = rnorm(200),
  Contrast_B = rnorm(200)
)
plot_brain_grid(atl, vals)

results <- data.frame(
  id = rev(atl$ids),
  Contrast_A = rnorm(200),
  Contrast_B = rnorm(200)
)
plot_brain_grid(
  atl,
  data = results,
  values = c("Contrast_A", "Contrast_B")
)
} # }
```

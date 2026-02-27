# Multi-panel Brain Plot Grid

Arranges multiple brain surface plots into a grid layout using
patchwork. Each element of `vals_list` produces one panel rendered by
[`plot_brain`](plot_brain.md).

## Usage

``` r
plot_brain_grid(
  surfatlas,
  vals_list,
  views = c("lateral", "medial"),
  hemis = c("left", "right"),
  ncol = NULL,
  shared_scale = TRUE,
  palette = "cork",
  lim = NULL,
  titles = NULL,
  colorbar = TRUE,
  ...
)
```

## Arguments

- surfatlas:

  A `surfatlas` object (e.g., from [`schaefer_surf`](schaefer_surf.md)
  or [`glasser_surf`](glasser_surf.md)).

- vals_list:

  A named list of numeric vectors, one per panel. Each vector must have
  length equal to the number of atlas regions.

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

  Logical. If `TRUE` (default), a standalone colorbar panel is appended
  to the grid.

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
} # }
```

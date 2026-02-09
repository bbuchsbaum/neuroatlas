# Plot Brain Surface Atlas

Renders a triangle-mesh projection of cortical surface parcellations
with configurable views and optional ggiraph interactivity. This
function replaces the legacy ggseg-based visualisation pipeline.

## Usage

``` r
plot_brain(
  surfatlas,
  vals = NULL,
  views = c("lateral", "medial"),
  hemis = c("left", "right"),
  surface = "inflated",
  color_method = "rule_hcl",
  colors = NULL,
  palette = "cork",
  lim = NULL,
  interactive = TRUE,
  ncol = 2L,
  border = TRUE,
  border_color = "grey30",
  border_size = 0.15,
  border_lineend = "round",
  border_linejoin = "round",
  silhouette = border,
  silhouette_color = border_color,
  silhouette_size = border_size,
  network_border = FALSE,
  network_border_color = border_color,
  network_border_size = border_size * 2,
  outline = FALSE,
  bg = "white",
  ...
)
```

## Arguments

- surfatlas:

  A surface atlas object of class `"surfatlas"` (e.g. from
  [`schaefer_surf()`](schaefer_surf.md) or
  [`glasser_surf()`](glasser_surf.md)).

- vals:

  Optional numeric vector of values to map onto parcels. Length must
  equal the number of atlas regions (`length(surfatlas$ids)`). When
  `NULL` (default), parcels are coloured using the ROI colour system.

- views:

  Character vector of views to render. Any combination of `"lateral"`,
  `"medial"`, `"dorsal"`, `"ventral"`. Default:
  `c("lateral", "medial")`.

- hemis:

  Character vector of hemispheres to include. Default:
  `c("left", "right")`.

- surface:

  Surface type. One of `"inflated"`, `"pial"`, `"white"`. Must match the
  surface type of `surfatlas`.

- color_method:

  Colour algorithm for discrete parcel colouring (when `vals` is
  `NULL`). Passed to [`atlas_roi_colors()`](atlas_roi_colors.md).
  Default: `"rule_hcl"`.

- colors:

  Optional pre-computed colours: a tibble with `id` and `color` columns,
  or a named character vector of hex colours keyed by region ID.
  Overrides `color_method` when `vals` is `NULL`.

- palette:

  Character: scico palette for continuous colour scale (when `vals` is
  provided). Default: `"cork"`.

- lim:

  Numeric vector of length 2 for colour scale limits (continuous mode).
  Defaults to range of `vals`.

- interactive:

  Logical. If `TRUE` (default), returns a
  [`ggiraph::girafe`](https://davidgohel.github.io/ggiraph/reference/girafe.html)
  widget with hover tooltips. If `FALSE`, returns a static `ggplot2`
  object.

- ncol:

  Integer: number of columns in the facet layout. Default: 2.

- border:

  Logical. If `TRUE` (default), draw thin lines at parcel boundaries
  (edges between different parcels). Gives a clean ggseg-like
  appearance.

- border_color:

  Colour for parcel boundary lines. Default: `"grey30"`.

- border_size:

  Line width for parcel boundaries. Default: `0.15`.

- border_lineend:

  Line end style for boundary segments (passed to
  [`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)).
  One of `"butt"`, `"round"`, `"square"`. Default: `"round"`.

- border_linejoin:

  Line join style for boundary segments (passed to
  [`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)).
  One of `"round"`, `"mitre"`, `"bevel"`. Default: `"round"`.

- silhouette:

  Logical. If `TRUE`, draw the mesh silhouette (edges between visible
  and culled faces) as a separate boundary layer. Defaults to `border`.

- silhouette_color:

  Colour for silhouette lines. Default: `border_color`.

- silhouette_size:

  Line width for silhouette lines. Default: `border_size`.

- network_border:

  Logical. If `TRUE`, highlight boundaries between different networks
  (requires `surfatlas$network`). Default: `FALSE`.

- network_border_color:

  Colour for network boundary lines. Default: `border_color`.

- network_border_size:

  Line width for network boundary lines. Default: `border_size * 2`.

- outline:

  Logical. If `TRUE`, draw every triangle edge (mesh wireframe).
  Default: `FALSE`. Typically `border` is preferred.

- bg:

  Character: background colour for the plot. Default: `"white"`.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot2` object (when `interactive = FALSE`) or a
[`ggiraph::girafe`](https://davidgohel.github.io/ggiraph/reference/girafe.html)
widget (when `interactive = TRUE`).

## Examples

``` r
if (FALSE) { # \dontrun{
atl <- schaefer_surf(200, 17)
plot_brain(atl)
plot_brain(atl, vals = rnorm(200), palette = "vik")
plot_brain(atl, views = "lateral", interactive = FALSE)

# Styling: rounded white parcel borders + thicker silhouette + network edges
plot_brain(
  atl,
  interactive = FALSE,
  border_color = "white",
  border_size = 0.25,
  border_lineend = "round",
  silhouette_size = 0.6,
  network_border = TRUE,
  network_border_color = "grey10",
  network_border_size = 0.5,
  bg = "#f7f7f7"
)
} # }
```

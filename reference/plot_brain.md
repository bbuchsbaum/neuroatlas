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
  data_id_mode = c("parcel", "polygon"),
  ncol = 2L,
  panel_layout = c("native", "presentation"),
  style = c("default", "ggseg_like"),
  border = TRUE,
  border_geom = c("path", "segment"),
  boundary_smooth = 0L,
  projection_smooth = 0L,
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
  shading = FALSE,
  shading_strength = 0.22,
  shading_gamma = 1,
  shading_color = "black",
  fill_alpha = 1,
  overlay = NULL,
  overlay_threshold = NULL,
  overlay_alpha = 0.45,
  overlay_palette = "vik",
  overlay_lim = NULL,
  overlay_border = TRUE,
  overlay_border_color = "black",
  overlay_border_size = 0.25,
  overlay_fun = c("avg", "nn", "mode"),
  overlay_sampling = c("midpoint", "normal_line", "thickness"),
  colorbar = FALSE,
  colorbar_title = NULL,
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

- data_id_mode:

  Interactive data-id granularity (when `interactive = TRUE`):
  `"parcel"` (default) uses parcel ids; `"polygon"` encodes panel +
  parcel + polygon/face id for click-to-surface workflows.

- ncol:

  Integer: number of columns in the facet layout. Default: 2.

- panel_layout:

  Panel coordinate layout strategy: `"native"` (default) preserves raw
  projected units; `"presentation"` recentres each panel, rotates
  dorsal/ventral views to horizontal, and normalises per-panel scale for
  a cleaner ggseg-like grid.

- style:

  Visual preset. `"default"` keeps existing behaviour. `"ggseg_like"`
  enables a cleaner publication style and, unless explicitly overridden,
  switches `panel_layout` to `"presentation"` with softer border
  defaults and light projection smoothing.

- border:

  Logical. If `TRUE` (default), draw thin lines at parcel boundaries
  (edges between different parcels). Gives a clean ggseg-like
  appearance.

- border_geom:

  Boundary rendering method. `"path"` (default) chains boundary edges
  into longer paths for smoother lines; `"segment"` draws each boundary
  edge independently.

- boundary_smooth:

  Non-negative integer controlling Chaikin smoothing iterations applied
  to boundary paths when `border_geom = "path"`. `0` (default) keeps
  original mesh-aligned boundaries; `1` or `2` yields cleaner curves in
  close-up figures.

- projection_smooth:

  Non-negative integer controlling Laplacian-like smoothing iterations
  applied to projected vertex coordinates before parcel polygons are
  constructed. This smooths filled parcel edges while preserving shared
  boundaries across parcels. `0` (default) keeps raw projected
  coordinates.

- border_color:

  Colour for parcel boundary lines. Default: `"grey30"`.

- border_size:

  Line width for parcel boundaries. Default: `0.15`.

- border_lineend:

  Line end style for boundary lines (passed to
  [`geom_path`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  /
  [`geom_segment`](https://ggplot2.tidyverse.org/reference/geom_segment.html)).
  One of `"butt"`, `"round"`, `"square"`. Default: `"round"`.

- border_linejoin:

  Line join style for boundary lines (passed to
  [`geom_path`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  /
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

- shading:

  Logical. If `TRUE`, overlay a subtle normal-based shading layer to
  enhance depth cues (recommended for static figures).

- shading_strength:

  Numeric in `[0, 1]`. Maximum opacity of the shading overlay. Default:
  `0.22`.

- shading_gamma:

  Positive numeric scalar controlling the shadow falloff. Higher values
  concentrate shadows in more oblique regions. Default: `1`.

- shading_color:

  Colour of the shading overlay. Default: `"black"`.

- fill_alpha:

  Numeric in `[0, 1]`. Opacity of parcel fills. Lower values can help
  the shading read more clearly. Default: `1`.

- overlay:

  Vertex-wise overlay or a `NeuroVol`. If a `NeuroVol`, it is
  automatically projected onto the surface using
  [`neurosurf::vol_to_surf()`](https://rdrr.io/pkg/neurosurf/man/vol_to_surf.html).
  Otherwise, a list with `lh` and `rh` components (numeric vectors
  matching the vertex count of each hemisphere mesh).

- overlay_threshold:

  Optional absolute threshold for overlay values before rendering.

- overlay_alpha:

  Numeric in `[0, 1]`. Opacity of overlay polygons. Default: `0.45`.

- overlay_palette:

  scico palette for overlay colour mapping. Default: `"vik"`.

- overlay_lim:

  Optional numeric length-2 limits for overlay colour mapping.

- overlay_border:

  Logical. If `TRUE`, draw cluster overlay boundaries. Default: `TRUE`.

- overlay_border_color:

  Colour for overlay boundaries. Default: `"black"`.

- overlay_border_size:

  Line width for overlay boundaries. Default: `0.25`.

- overlay_fun:

  Character: interpolation function passed to
  [`neurosurf::vol_to_surf()`](https://rdrr.io/pkg/neurosurf/man/vol_to_surf.html)
  when `overlay` is a `NeuroVol`. One of `"avg"`, `"nn"`, or `"mode"`.
  Default: `"avg"`.

- overlay_sampling:

  Character: sampling strategy passed to
  [`neurosurf::vol_to_surf()`](https://rdrr.io/pkg/neurosurf/man/vol_to_surf.html)
  when `overlay` is a `NeuroVol`. One of `"midpoint"`, `"normal_line"`,
  or `"thickness"`. Default: `"midpoint"`.

- colorbar:

  Logical. When `vals` is non-NULL and `interactive = FALSE`, add a
  standalone colorbar panel composed alongside the main plot via
  patchwork. Default: `FALSE`.

- colorbar_title:

  Optional character label for the colorbar.

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
  shading = TRUE,
  fill_alpha = 0.98,
  bg = "#f7f7f7"
)
} # }
```

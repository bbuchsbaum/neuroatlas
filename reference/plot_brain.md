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
  static_backend = c("ggplot", "cpu"),
  data_id_mode = c("parcel", "polygon"),
  ncol = 2L,
  panel_layout = c("native", "presentation"),
  style = c("default", "ggseg_like", "stat_publication"),
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
  outer_contour = FALSE,
  outer_contour_color = "grey35",
  outer_contour_size = 0.3,
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
  overlay_alpha_mode = c("constant", "threshold"),
  overlay_alpha_ramp = NULL,
  overlay_palette = "vik",
  overlay_lim = NULL,
  overlay_border = FALSE,
  overlay_border_color = "black",
  overlay_border_size = 0.25,
  overlay_fun = c("avg", "nn", "mode"),
  overlay_sampling = c("midpoint", "normal_line", "thickness"),
  overlay_interpolation = c("legacy", "nearest", "linear"),
  overlay_aggregate = NULL,
  overlay_n_samples = NULL,
  overlay_depth = NULL,
  overlay_surface_smooth_fwhm = 0,
  colorbar = FALSE,
  colorbar_source = c("auto", "base", "overlay", "none"),
  colorbar_title = NULL,
  overlay_title = colorbar_title,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  panel_labels = NULL,
  cortex_mask = NULL,
  cortex_mask_source = NULL,
  anatomy_metric = NULL,
  anatomy_metric_source = NULL,
  medial_wall = c("shade", "mask", "outline"),
  camera = c("canonical", "presentation"),
  orientation_labels = TRUE,
  render_width = 1200L,
  render_height = 750L,
  render_antialias = 2L,
  outline = FALSE,
  background = FALSE,
  background_color = "grey80",
  depth_cull = TRUE,
  bg = "white",
  data = NULL,
  value = NULL,
  by = NULL,
  allow_partial = FALSE,
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

  Surface type. One of `"inflated"`, `"pial"`, `"white"`, or
  `"midthickness"`. When omitted, it is read from `surfatlas$surf_type`;
  an explicitly requested type must match the geometry carried by
  `surfatlas`.

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

- static_backend:

  Static renderer: the existing `"ggplot"` polygon path or deterministic
  `"cpu"` barycentric rasterization. The CPU path is intended for
  continuous publication overlays and requires no OpenGL or browser.

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
  defaults and light projection smoothing. `"stat_publication"` treats
  the surface as an anatomical substrate for a continuous overlay:
  parcel and culling-derived silhouette lines are disabled, weak shading
  is drawn below the overlay, and a clean outer contour is enabled.

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

- outer_contour:

  Logical. If `TRUE`, draw the largest projected exterior loop in each
  panel. Unlike `silhouette`, this excludes internal visibility and
  sulcal edge fragments.

- outer_contour_color:

  Colour for the exterior contour.

- outer_contour_size:

  Line width for the exterior contour.

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

  Vertex-wise overlay or a `NeuroVol`. If a `NeuroVol`, it is projected
  onto the surface using
  [`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html).
  A raw `NeuroVol` does not carry a template-space identifier that this
  function can enforce, so callers must ensure it is already expressed
  in the coordinates of the resolved white and pial surfaces. No
  cross-coordinate transform is applied here. Otherwise, a list with
  `lh` and `rh` components (numeric vectors matching the vertex count of
  each hemisphere mesh).

- overlay_threshold:

  Optional absolute threshold for overlay values before rendering.

- overlay_alpha:

  Numeric in `[0, 1]`. Opacity of overlay polygons. Default: `0.45`.

- overlay_alpha_mode:

  Character. `"constant"` uses `overlay_alpha` for all rendered overlay
  faces. `"threshold"` fades overlay faces from transparent at
  `overlay_threshold` to `overlay_alpha`, avoiding hard threshold edges
  for dense surface maps. Default: `"constant"`.

- overlay_alpha_ramp:

  Optional positive numeric scalar controlling the absolute-value
  distance above `overlay_threshold` over which
  `overlay_alpha_mode = "threshold"` reaches full opacity. If `NULL`, a
  small data-driven ramp is chosen from the rendered overlay values. Use
  `0` to disable ramping while keeping the threshold mode.

- overlay_palette:

  scico palette for overlay colour mapping. Default: `"vik"`.

- overlay_lim:

  Optional numeric length-2 limits for overlay colour mapping.

- overlay_border:

  Logical. If `TRUE`, draw cluster overlay boundaries. Default: `FALSE`.

- overlay_border_color:

  Colour for overlay boundaries. Default: `"black"`.

- overlay_border_size:

  Line width for overlay boundaries. Default: `0.25`.

- overlay_fun:

  Character: interpolation function passed to
  [`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html)
  when `overlay` is a `NeuroVol`. One of `"avg"`, `"nn"`, or `"mode"`.
  Default: `"avg"`.

- overlay_sampling:

  Character: sampling strategy passed to
  [`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html)
  when `overlay` is a `NeuroVol`. One of `"midpoint"`, `"normal_line"`,
  or `"thickness"`. Default: `"midpoint"`.

- overlay_interpolation:

  Voxel interpolation passed to
  [`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html):
  `"legacy"`, `"nearest"`, or `"linear"`. The publication preset
  defaults to linear interpolation.

- overlay_aggregate:

  Optional explicit aggregation across depth samples: `"mean"`,
  `"mode"`, or `"closest"`.

- overlay_n_samples:

  Optional number of sampling depths.

- overlay_depth:

  Optional explicit thickness fractions or normal-line offsets. The
  publication preset uses five fractions from 0.1 through 0.9.

- overlay_surface_smooth_fwhm:

  Tangential surface smoothing in mm. Defaults to zero; it is separate
  from voxel interpolation.

- colorbar:

  Logical or character. When `interactive = FALSE`, controls whether and
  where to add a standalone colorbar panel. Use `TRUE` or `"right"` for
  a vertical colorbar, `"bottom"` for a horizontal colorbar, or `FALSE`
  / `"none"` to omit it. Default: `FALSE`.

- colorbar_source:

  Which mapped quantity supplies the static colorbar: `"auto"`,
  `"base"`, `"overlay"`, or `"none"`. `"auto"` chooses the overlay
  whenever one was supplied, even if all of its values are removed by
  thresholding; otherwise it chooses `vals`.

- colorbar_title:

  Optional character label for the colorbar.

- overlay_title:

  Optional character label used when the colorbar source is the overlay.
  Defaults to `colorbar_title`.

- title, subtitle, caption:

  Optional plot-level annotations for static output. When a colorbar is
  present these are applied to the composed figure; otherwise they are
  added directly to the returned ggplot.

- panel_labels:

  Optional panel label override. Use either an unnamed character vector
  matching the number of panels, a named character vector keyed by
  default panel names such as `"Left Lateral"`, or a function that takes
  the default panel name and returns a new label.

- cortex_mask:

  Optional logical vertex-domain mask or lh/rh list.

- cortex_mask_source:

  Provenance label for an explicit cortex mask.

- anatomy_metric:

  Optional anatomy metric or lh/rh list. A declared sulcal metric is
  preferred; otherwise the CPU backend computes curvature on matched
  white geometry and verifies vertex correspondence.

- anatomy_metric_source:

  Provenance label for an explicit metric.

- medial_wall:

  Explicit medial-wall policy: neutral shade, mask, or independent
  outline.

- camera:

  Strict canonical orthographic or slightly oblique presentation camera.

- orientation_labels:

  Draw small anterior/posterior marks in CPU panels.

- render_width, render_height:

  Per-panel CPU raster dimensions.

- render_antialias:

  CPU supersampling factor.

- outline:

  Logical. If `TRUE`, draw every triangle edge (mesh wireframe).
  Default: `FALSE`. Typically `border` is preferred.

- background:

  Logical. If `TRUE`, draw the full cortical surface beneath the
  parcellation, so sparse atlases (e.g. the Wang visual areas, which
  label only part of cortex) are shown in anatomical context rather than
  floating on the page. The backdrop is given sulcal/gyral depth via
  normal-based shading (controlled by `shading_strength`/
  `shading_gamma`), so it reads as a folded surface rather than a flat
  silhouette. Default: `FALSE`.

- background_color:

  Fill colour for the cortex backdrop when `background = TRUE`. Default:
  `"grey80"`.

- depth_cull:

  Logical. If `TRUE` (default), remove faces hidden behind nearer
  cortical surface faces in each projected view. This makes static
  medial/lateral panels read as opaque cortex instead of showing
  far-side folds through the mesh.

- bg:

  Character: background colour for the plot. Default: `"white"`.

- data:

  Optional data frame, tibble, or `parcel_data` object with one row per
  parcel. When supplied, `value` is aligned to the atlas with
  [`align_parcel_values()`](align_parcel_values.md) before rendering.
  Supply either `data` or `vals`, not both.

- value:

  Numeric column in `data`, supplied as a bare name or character string.

- by:

  Parcel-key specification for `data`. Use a shared column name such as
  `"id"`, or map an atlas key to a differently named data column with
  `c(id = "roi_index")`. Composite keys are supported. When `NULL`, a
  safe unique key is inferred.

- allow_partial:

  Logical. If `FALSE` (default), `data` must contain every atlas parcel.
  If `TRUE`, missing parcels are rendered with `NA` values. Unknown and
  duplicate keys always error.

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot2` object (when `interactive = FALSE` and no standalone
colorbar is requested), a `patchwork` object (when a standalone static
colorbar is composed), or a
[`ggiraph::girafe`](https://davidgohel.github.io/ggiraph/reference/girafe.html)
widget (when `interactive = TRUE`).

## Examples

``` r
if (FALSE) { # \dontrun{
atl <- schaefer_surf(200, 17)
plot_brain(atl)
plot_brain(atl, vals = rnorm(200), palette = "vik")
plot_brain(atl, views = "lateral", interactive = FALSE)

results <- data.frame(
  roi_index = rev(atl$ids),
  estimate = seq(-2, 2, length.out = length(atl$ids))
)
plot_brain(
  atl,
  data = results,
  value = estimate,
  by = c(id = "roi_index"),
  views = "medial",
  hemis = "left",
  interactive = FALSE
)

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

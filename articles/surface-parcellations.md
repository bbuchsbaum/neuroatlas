# Surface Parcellations with neurosurf

A surface parcellation answers a simple-looking question: *which
cortical parcel owns each mesh vertex?* The answer involves three
aligned pieces: surface geometry, one integer label per vertex, and
region metadata. This article shows how `neuroatlas` keeps those pieces
together and how to attach one statistic per parcel without changing the
atlas.

The first surface-atlas call may download annotation files. For that
reason, network-backed chunks are displayed but not run during package
builds. Their outputs are real committed figures, generated from the
code shown here.

## What is a surface atlas?

[`schaefer_surf()`](../reference/schaefer_surf.md) returns a
`surfatlas`. Its two labelled surfaces share one region catalogue:

| Object | Contents | Cardinality |
|----|----|----|
| `atl$lh_atlas` | left geometry plus an integer label at every vertex | vertices |
| `atl$rh_atlas` | right geometry plus an integer label at every vertex | vertices |
| `atl$ids`, `atl$labels`, `atl$hemi` | region identity and metadata | parcels |

Load a 200-parcel Schaefer atlas on the bundled `fsaverage6` geometry
like this:

``` r

library(neuroatlas)

atl <- schaefer_surf(
  parcels = 200,
  networks = 7,
  space = "fsaverage6",
  surf = "inflated"
)

class(atl)
class(atl$lh_atlas)
```

The mesh geometry is bundled, while the Schaefer annotation is
downloaded and cached on first use. Annotations supply the vertex
labels; geometry alone is not a parcellation.

## How do you verify geometry and labels?

A grey silhouette proves only that a mesh can be projected. A useful
diagnostic checks topology, label length, and region coverage together:

``` r

lh_geometry <- neurosurf::geometry(atl$lh_atlas)
lh_labels <- as.integer(atl$lh_atlas@data)

diagnostic <- c(
  vertices = nrow(neurosurf::vertices(lh_geometry)),
  faces = ncol(lh_geometry@mesh$it),
  vertex_labels = length(lh_labels),
  labelled_regions = length(unique(lh_labels[lh_labels > 0]))
)
diagnostic

stopifnot(
  diagnostic[["vertices"]] == diagnostic[["vertex_labels"]],
  diagnostic[["faces"]] > 0,
  diagnostic[["labelled_regions"]] == sum(atl$hemi == "left"),
  all(unique(lh_labels[lh_labels > 0]) %in% atl$ids)
)
```

The resulting parcellation should look like this, with visible parcel
boundaries and more than one label colour:

``` r

plot_brain(
  atl,
  vals = seq(-2.5, 2.5, length.out = length(atl$ids)),
  views = c("lateral", "medial"),
  interactive = FALSE,
  style = "ggseg_like",
  colorbar = "right",
  colorbar_title = "Example value",
  title = "Schaefer-200 (7 networks) on fsaverage6"
)
```

![Schaefer 200-parcel atlas on left and right inflated fsaverage6
surfaces in lateral and medial views. Parcels have distinct
blue-to-orange values, visible white boundaries, and a vertical
example-value colorbar.](figures/overview-schaefer-surface.png)

## How do you attach one value per parcel?

Keep the atlas and the data conceptually separate. A numeric vector
supplied to [`plot_brain()`](../reference/plot_brain.md) must have
exactly one value for every element of `atl$ids`:

``` r

parcel_values <- seq(-2, 2, length.out = length(atl$ids))
stopifnot(length(parcel_values) == length(atl$ids))

plot_brain(
  atl,
  vals = parcel_values,
  views = c("lateral", "medial"),
  interactive = FALSE,
  style = "ggseg_like",
  colorbar = "bottom",
  colorbar_title = "Standardized effect"
)
```

[`map_atlas()`](../reference/map_atlas.md) is the tabular companion to
this plot. It does **not** mutate the surface or return a labelled mesh:

``` r

mapped <- map_atlas(atl, parcel_values)
mapped

stopifnot(
  nrow(mapped) == length(atl$ids),
  identical(mapped$statistic, parcel_values),
  identical(mapped$region, atl$labels),
  identical(mapped$hemi, atl$hemi)
)
```

Use the tibble for modelling and reporting; pass the original atlas plus
the same value vector to [`plot_brain()`](../reference/plot_brain.md)
for a surface figure.

## How do you extract one surface ROI?

[`get_roi()`](../reference/get_roi.md) selects vertices for one or more
named regions and returns
[`neurosurf::ROISurface`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface.html)
objects. Labels may occur in both hemispheres, so make the side explicit
when the question is unilateral:

``` r

region_name <- atl$labels[atl$hemi == "left"][1]
roi <- get_roi(atl, label = region_name, hemi = "left")

stopifnot(
  length(roi) == 1L,
  methods::is(roi[[1]], "ROISurface"),
  length(roi[[1]]) > 0L
)
```

Atlas-level subsetting is currently volume-only.
`filter_atlas(atl, ...)` and `sub_atlas(atl, ...)` fail clearly for a
surface atlas; use [`get_roi()`](../reference/get_roi.md) for surface
regions rather than expecting a smaller `surfatlas`.

## How do you use per-vertex data?

Parcel values and vertex values are different data shapes. For an
already surface-aligned continuous field, supply one vector per
hemisphere through `overlay`:

``` r

vertex_values <- list(
  lh = rep(0, nrow(neurosurf::vertices(
    neurosurf::geometry(atl$lh_atlas)
  ))),
  rh = rep(0, nrow(neurosurf::vertices(
    neurosurf::geometry(atl$rh_atlas)
  )))
)

plot_brain(
  atl,
  overlay = vertex_values,
  interactive = FALSE,
  style = "stat_publication",
  colorbar = "bottom",
  overlay_title = "Vertex statistic"
)
```

A `NeuroVol` is not automatically aligned merely because it can be
sampled. The current [`plot_brain()`](../reference/plot_brain.md) volume
projection cannot infer template identity from a raw `NeuroVol`, check
compatibility, or consume a transformed white/pial pair. Passing a
volume is therefore unchecked caller responsibility and is appropriate
only when the volume already uses the resolved surface coordinates.
`fsaverage`, `fsaverage5`, and `fsaverage6` use MNI305 coordinates,
whereas most modern MNI volumes use MNI152 coordinates; do not project
the latter directly.
[`transform_vertices_to_volume()`](../reference/transform_vertices_to_volume.md)
is useful for coordinate-level calculations, but it does not by itself
create a [`plot_brain()`](../reference/plot_brain.md) projection
geometry. A grid reslice does not establish correspondence.

## What about other atlases and templates?

[`glasser_surf()`](../reference/glasser_surf.md) has the same
`surfatlas` contract, but uses 164k `fsaverage` geometry and downloads
both geometry and annotations on first use. Raw geometry from
[`load_surface_template()`](../reference/load_surface_template.md) has
no atlas labels; the next article explains that distinction in detail.

Continue with:

1.  [`vignette("surface-panels")`](../articles/surface-panels.md) for
    shared legends and multi-map figures.
2.  [`vignette("surface-templates")`](../articles/surface-templates.md)
    for paths, geometry, and per-vertex data.
3.  [`vignette("working-with-templateflow")`](../articles/working-with-templateflow.md)
    for live asset discovery.

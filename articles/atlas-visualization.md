# Atlas Visualization with Optimal Colours

Every atlas in neuroatlas can be visualised with a single call to
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). Behind the
scenes, [`plot.atlas()`](../reference/plot-methods.md) renders coloured
parcels as volumetric slices using **neuroim2**’s `plot_montage()` and
`plot_ortho()`, with colours assigned automatically by the
**roi_colors** system.

``` r

library(neuroatlas)
```

## Quick Start

``` r

atlas <- get_aseg_atlas()
plot(atlas)
```

![Axial montage of the bundled FreeSurfer ASEG atlas, with subcortical
and midline regions shown in distinct colours across twelve
slices.](atlas-visualization_files/figure-html/quick-montage-1.png)

The default view is a multi-slice **montage** (axial slices) with
colours chosen by the `rule_hcl` algorithm — a fast, deterministic
palette that uses network hues and hemisphere luminance differences.

For a three-plane **orthogonal** view:

``` r

plot(atlas, view = "ortho")
```

![Orthogonal sagittal, coronal, and axial planes through the bundled
ASEG atlas, with each anatomical region in a distinct
colour.](atlas-visualization_files/figure-html/quick-ortho-1.png)![Orthogonal
sagittal, coronal, and axial planes through the bundled ASEG atlas, with
each anatomical region in a distinct
colour.](atlas-visualization_files/figure-html/quick-ortho-2.png)

### Region legends

By default no legend is drawn — for a 400-region cortical parcellation
it would be useless. But for small atlases (subcortical, MTL, a handful
of ROIs) a colour legend is genuinely helpful. Pass `legend = TRUE` to
add one below a montage; labels appearing in both hemispheres are
disambiguated with `(L)`/`(R)`:

``` r

plot(atlas, legend = TRUE)   # ASEG has 17 regions
```

![Axial ASEG montage with a compact region legend below the slices; left
and right occurrences of repeated labels are
distinguished.](atlas-visualization_files/figure-html/quick-legend-1.png)

The legend is capped by `legend_max` (default 30). A request above that
limit emits a warning and omits the legend; raise `legend_max` to
override. Legends are drawn for montage views, not orthogonal views
whose planes contain different subsets of regions.

## Colour Algorithms

neuroatlas ships four colour algorithms, each suited to different use
cases. Pass the `method` argument to
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) to switch
between them.

### rule_hcl (default)

Deterministic and fast. Assigns hues per network with anterior-posterior
gradients and hemisphere luminance offsets.

``` r

plot(atlas, method = "rule_hcl", nslices = 8)
```

![Eight-slice ASEG montage using deterministic rule-based HCL colours to
separate neighbouring anatomical
regions.](atlas-visualization_files/figure-html/rule-hcl-1.png)

### maximin_view

Optimises perceptual separation between spatially neighbouring ROIs
across slice views. Best for publication figures where adjacent parcels
must be easily distinguished.

``` r

plot(atlas, method = "maximin_view", nslices = 8)
```

![Eight-slice ASEG montage using maximin colours selected to increase
separation between spatially adjacent
regions.](atlas-visualization_files/figure-html/maximin-1.png)

### network_harmony

Network-aware: ROIs in the same network share analogous hue families
while still maximising local separation. Requires the atlas to have a
`$network` field (e.g. Schaefer atlases).

``` r

# Requires a Schaefer atlas with network metadata (network download)
schaefer <- get_schaefer_atlas(parcels = "200", networks = "7")
plot(schaefer, method = "network_harmony", nslices = 8)
```

### embedding

Projects ROI features to 2D (PCA or UMAP) and maps polar angle to hue,
yielding globally structured gradients.

``` r

plot(atlas, method = "embedding", nslices = 8)
```

![Eight-slice ASEG montage using colours derived from a two-dimensional
embedding of region
features.](atlas-visualization_files/figure-html/embedding-1.png)

## Custom Colours

You can supply your own colours as a named character vector (names are
region IDs) or as a tibble from
[`atlas_roi_colors()`](../reference/atlas_roi_colors.md).

### Named vector

``` r

my_cols <- setNames(rainbow(length(atlas$ids)), atlas$ids)
plot(atlas, colors = my_cols, nslices = 6)
```

![Six-slice ASEG montage using a caller-supplied rainbow colour for each
named region
ID.](atlas-visualization_files/figure-html/custom-named-1.png)

### Pre-computed tibble

``` r

color_tbl <- atlas_roi_colors(atlas, method = "maximin_view")
head(color_tbl)
#> # A tibble: 6 × 2
#>      id color  
#>   <int> <chr>  
#> 1    10 #14E2C6
#> 2    11 #EEB8C7
#> 3    12 #A8C3E3
#> 4    13 #F6BA4F
#> 5    16 #81C2FF
#> 6    17 #87CCBE
plot(atlas, colors = color_tbl, nslices = 6)
```

![Six-slice ASEG montage reusing a precomputed table of maximin region
colours.](atlas-visualization_files/figure-html/custom-tibble-1.png)

## Programmatic Colour Access

The [`atlas_roi_colors()`](../reference/atlas_roi_colors.md) function is
the bridge between atlas objects and the `roi_colors_*()` family. It
extracts ROI centroids, builds a metadata tibble, and dispatches to the
requested algorithm.

``` r

cols <- atlas_roi_colors(atlas, method = "rule_hcl")
cols
#> # A tibble: 17 × 2
#>       id color  
#>    <int> <chr>  
#>  1    10 #FC90AD
#>  2    11 #EAA06D
#>  3    12 #F19B7F
#>  4    13 #F99596
#>  5    16 #EB7DAD
#>  6    17 #FD8EB8
#>  7    18 #F79690
#>  8    26 #ED9E73
#>  9    28 #E98292
#> 10    49 #D97088
#> 11    50 #C28439
#> 12    51 #CC7D56
#> 13    52 #D3776A
#> 14    53 #DA6E93
#> 15    54 #D17963
#> 16    58 #C58241
#> 17    60 #D7737C
```

This tibble can be joined with other atlas metadata for downstream
analyses.

## Controlling Slice Count

Use `nslices` to control how many slices appear in the montage:

``` r

plot(atlas, nslices = 4)
```

![Compact four-slice ASEG montage demonstrating control of the number of
displayed axial
sections.](atlas-visualization_files/figure-html/nslices-1.png)

## Surface Figures with Layout Control

For cortical surface atlases,
[`plot_brain()`](../reference/plot_brain.md) gives you direct control
over the static figure layout. You can move the colorbar, add figure
titles, and replace the default facet labels without assembling the
figure by hand afterward.

``` r

surf_atl <- schaefer_surf(
  parcels = 200,
  networks = 7,
  space = "fsaverage6",
  surf = "inflated"
)

surf_vals <- seq(-2, 2, length.out = length(surf_atl$ids))

plot_brain(
  surf_atl,
  vals = surf_vals,
  views = c("lateral", "medial"),
  interactive = FALSE,
  style = "ggseg_like",
  colorbar = "bottom",
  colorbar_title = "Standardized effect",
  title = "Parcel-level summary on fsaverage6",
  subtitle = "Bottom colorbar plus concise panel labels",
  panel_labels = c(
    "Left Lateral" = "LH lateral",
    "Right Lateral" = "RH lateral",
    "Left Medial" = "LH medial",
    "Right Medial" = "RH medial"
  )
)
```

![Schaefer parcels on left and right inflated hemispheres in lateral and
medial views, coloured by one value per parcel with a horizontal
effect-size colorbar.](figures/surface-panel-parcels.png)

Because this example mixes lateral and medial views, each short label
retains both hemisphere and view. For a dedicated guide to multi-panel
layout, shared legends, and the default hemisphere-orientation
convention, see
[`vignette("surface-panels", package = "neuroatlas")`](../articles/surface-panels.md).

## Which entry point should you use?

Use [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for volume
atlases and [`plot_brain()`](../reference/plot_brain.md) for surface
atlases. The older ggseg helpers are migration paths:
[`ggseg_schaefer()`](../reference/ggseg_schaefer.md) is deprecated and
[`plot_glasser()`](../reference/plot_glasser.md) has been removed with a
stop-level deprecation. New code should load a surface atlas with
[`schaefer_surf()`](../reference/schaefer_surf.md) or
[`glasser_surf()`](../reference/glasser_surf.md) and pass it to
[`plot_brain()`](../reference/plot_brain.md).

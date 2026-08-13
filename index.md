# neuroatlas ![](reference/figures/logo.png)

**neuroatlas** provides a unified interface for working with
neuroimaging atlases and parcellations in R. Whether you’re conducting
ROI-based analyses, visualizing brain data, or integrating different
parcellation schemes, neuroatlas streamlines these tasks with
consistent, user-friendly functions.

## Features

- **Many built-in atlases**: Schaefer (100-1000 parcels), Brainnetome
  (246 regions), Glasser (360 regions), Harvard-Oxford, Julich-Brain,
  FreeSurfer ASEG, harmonized TemplateFlow/AtlasPack subcortical
  atlases, Olsen MTL, and probabilistic visual-cortex atlases (Wang
  2015, visfAtlas, cytoarchitectonic V1-V5)
- **Surface & volume**: Work with both volumetric and surface-based
  parcellations through one consistent interface
- **Atlas discovery**: Enumerate built-ins with
  [`list_atlases()`](reference/list_atlases.md) and load any of them by
  name with [`get_atlas()`](reference/get_atlas.md)
- **ROI analysis**: Extract and summarise regions with
  [`get_roi()`](reference/get_roi.md),
  [`map_atlas()`](reference/map_atlas.md),
  [`reduce_atlas()`](reference/reduce_atlas.md), and
  [`batch_reduce()`](reference/batch_reduce.md)
- **Atlas operations**: Combine and reshape parcellations with
  [`merge_atlases()`](reference/merge_atlases.md),
  [`filter_atlas()`](reference/filter_atlas.md),
  [`dilate_atlas()`](reference/dilate_atlas.md),
  [`atlas_overlap()`](reference/atlas_overlap.md), and resampling across
  spaces/resolutions
- **Spatial queries**: Look up parcels by world, voxel, or MNI
  coordinate with [`query_point()`](reference/query_point.md),
  [`query_coord()`](reference/query_coord.md), and
  [`query_vox()`](reference/query_coord.md)
- **Network & graph tools**:
  [`atlas_connectivity()`](reference/atlas_connectivity.md),
  [`atlas_graph()`](reference/atlas_graph.md) /
  [`as_igraph()`](reference/as_igraph.md),
  [`atlas_hierarchy()`](reference/atlas_hierarchy.md), and
  [`spin_test()`](reference/spin_test.md) spatial null models
- **TemplateFlow integration**: Access standardized templates through
  the pure-R `templateflow` backend
- **Visualization**: Publication-quality surface figures with
  [`plot_brain()`](reference/plot_brain.md) /
  [`plot_brain_grid()`](reference/plot_brain_grid.md),
  perceptually-optimised ROI palettes, the ggseg ecosystem, and an
  interactive [`cluster_explorer()`](reference/cluster_explorer.md)
  Shiny app
- **Provenance**: Every atlas carries structured space, family, and
  source metadata ([`atlas_ref()`](reference/atlas_ref.md),
  [`atlas_provenance()`](reference/atlas_provenance.md))

## Installation

You can install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("bbuchsbaum/neuroatlas")
```

### TemplateFlow Setup

TemplateFlow access uses the imported pure-R `templateflow` package. No
Python or reticulate setup is required:

``` r

neuroatlas::tflow_spaces(pattern = "^MNI")
neuroatlas::show_templateflow_cache_path()
```

## Quick Start

``` r

library(neuroatlas)

# Get a Schaefer atlas (200 parcels, 7 networks)
schaefer <- get_schaefer_atlas(parcels = 200, networks = 7)
print(schaefer)
#> ══ Schaefer Atlas ══════════════════════════════════════════════════════════════
#> Name: Schaefer-200-7networks
#> Dimensions: 182 x 218 x 182
#> Regions: 200
#> Networks: 7
#> Hemispheres: left: 100, right: 100
#> Unique networks: 7

# Extract a specific ROI by label (e.g. the first visual parcel)
roi <- get_roi(schaefer, "Vis_1")

# Get Glasser atlas
glasser <- get_glasser_atlas()

# Access templates via TemplateFlow
mni_brain <- get_template("MNI152NLin2009cAsym", variant = "brain")
```

### Discovering and loading atlases

``` r

# See every built-in atlas
list_atlases()

# Load any of them by id (with loader-specific arguments)
schaefer <- get_atlas("schaefer2018", parcels = "100", networks = "7")
```

## Palette demos

`neuroatlas` includes perceptually-optimised palettes for atlas ROIs.
For instance, you can generate a slice-aware palette for the Schaefer
200×7 atlas and feed it directly into
[`plot_brain()`](reference/plot_brain.md):

``` r

library(neuroatlas)

schaefer <- get_schaefer_atlas(parcels = 200, networks = 7)

colors <- atlas_roi_colors(
  schaefer,
  method = "maximin_view",
  seed = 1
)

schaefer_surface <- schaefer_surf(parcels = 200, networks = 7)

plot_brain(
  schaefer_surface,
  colors = colors,
  interactive = FALSE,
  style = "ggseg_like"
)
```

![Schaefer-200 (7-network) parcellation on the fsaverage6 surface with a
perceptually-optimised maximin colour palette; lateral and medial views
of both hemispheres.](reference/figures/README-roi-palette.png)

## Available Atlases

| Atlas | Function | Description |
|----|----|----|
| Schaefer | [`get_schaefer_atlas()`](reference/get_schaefer_atlas.md) | Cortical parcellations (100-1000 regions, 7 or 17 networks); surface via [`get_schaefer_surfatlas()`](reference/get_schaefer_surfatlas.md) |
| Brainnetome | [`get_brainnetome_atlas()`](reference/get_brainnetome_atlas.md) | 246-region connectional atlas with Yeo network and cytoarchitectonic metadata |
| Glasser | [`get_glasser_atlas()`](reference/get_glasser_atlas.md) | 360-region multi-modal cortical parcellation (surface via [`glasser_surf()`](reference/glasser_surf.md)) |
| Harvard-Oxford | [`get_harvard_oxford_atlas()`](reference/get_harvard_oxford_atlas.md) | Cortical/subcortical structural atlases via TemplateFlow or FSL |
| Julich-Brain | [`get_julich_brain_atlas()`](reference/get_julich_brain_atlas.md) | FSL Julich-Brain cytoarchitectonic atlas |
| ASEG | [`get_aseg_atlas()`](reference/get_aseg_atlas.md) | FreeSurfer subcortical segmentation |
| Subcortical | [`get_subcortical_atlas()`](reference/get_subcortical_atlas.md) | Harmonized thalamus, cerebellum, and subcortex atlases (AtlasPack/TemplateFlow) |
| Olsen MTL | [`get_olsen_mtl()`](reference/get_olsen_mtl.md) | Medial temporal lobe atlas with hippocampal subfields |
| Wang (2015) | [`get_wang_atlas()`](reference/get_wang_atlas.md) | Probabilistic visual topography on `fsaverage` (25 areas/hemi); probability volumes via [`get_wang_prob_atlas()`](reference/get_wang_prob_atlas.md) |
| visfAtlas | [`get_visfatlas()`](reference/get_visfatlas.md) | Probabilistic functional atlas of occipito-temporal visual cortex (33 regions) |
| Visual V1-V5 | [`get_visual_atlas()`](reference/get_visual_atlas.md) | Cytoarchitectonic early visual areas extracted from Julich-Brain |

## Documentation

- [Getting
  Started](https://bbuchsbaum.github.io/neuroatlas/articles/neuroatlas-overview.html) -
  Introduction and basic usage
- [Atlas Visualization with Optimal
  Colours](https://bbuchsbaum.github.io/neuroatlas/articles/atlas-visualization.html) -
  Perceptually-optimised ROI palettes
- [Surface Panel
  Figures](https://bbuchsbaum.github.io/neuroatlas/articles/surface-panels.html) -
  Static panel composition with
  [`plot_brain()`](reference/plot_brain.md) and
  [`plot_brain_grid()`](reference/plot_brain_grid.md)
- [Surface
  Templates](https://bbuchsbaum.github.io/neuroatlas/articles/surface-templates.html) -
  Geometry vs. data on surface meshes
- [Surface
  Parcellations](https://bbuchsbaum.github.io/neuroatlas/articles/surface-parcellations.html) -
  Surface-based atlas operations
- [Working with
  TemplateFlow](https://bbuchsbaum.github.io/neuroatlas/articles/working-with-templateflow.html) -
  Template access and management
- [Function
  Reference](https://bbuchsbaum.github.io/neuroatlas/reference/index.html) -
  Complete API documentation

## Related Packages

- [neuroim2](https://github.com/bbuchsbaum/neuroim2) - Core neuroimaging
  data structures
- [neurosurf](https://github.com/bbuchsbaum/neurosurf) - Surface-based
  operations
- [ggseg](https://github.com/ggsegverse/ggseg) - Brain visualization

## License

MIT © Bradley Buchsbaum

## Albers theme

This package uses the albersdown theme. Existing vignette theme hooks
are replaced so `albers.css` and local `albers.js` render consistently
on CRAN and GitHub Pages. The defaults are configured via
`params$family` and `params$preset` (family = ‘red’, preset =
‘interaction’). The pkgdown site uses
`template: { package: albersdown }` together with generated
`pkgdown/extra.css` and `pkgdown/extra.js` so the theme is linked and
activated on site pages.

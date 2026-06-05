
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neuroatlas <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bbuchsbaum/neuroatlas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bbuchsbaum/neuroatlas/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**neuroatlas** provides a unified interface for working with
neuroimaging atlases and parcellations in R. Whether you’re conducting
ROI-based analyses, visualizing brain data, or integrating different
parcellation schemes, neuroatlas streamlines these tasks with
consistent, user-friendly functions.

## Features

- **Many built-in atlases**: Schaefer (100-1000 parcels), Brainnetome
  (246 regions), Glasser (360 regions), Harvard-Oxford, Julich-Brain,
  FreeSurfer ASEG, harmonized TemplateFlow/AtlasPack subcortical atlases,
  Olsen MTL, and probabilistic visual-cortex atlases (Wang 2015,
  visfAtlas, cytoarchitectonic V1-V5)
- **Surface & volume**: Work with both volumetric and surface-based
  parcellations through one consistent interface
- **Atlas discovery**: Enumerate built-ins with `list_atlases()` and load
  any of them by name with `get_atlas()`
- **ROI analysis**: Extract and summarise regions with `get_roi()`,
  `map_atlas()`, `reduce_atlas()`, and `batch_reduce()`
- **Atlas operations**: Combine and reshape parcellations with
  `merge_atlases()`, `filter_atlas()`, `dilate_atlas()`,
  `atlas_overlap()`, and resampling across spaces/resolutions
- **Spatial queries**: Look up parcels by world, voxel, or MNI coordinate
  with `query_point()`, `query_coord()`, and `query_vox()`
- **Network & graph tools**: `atlas_connectivity()`, `atlas_graph()` /
  `as_igraph()`, `atlas_hierarchy()`, and `spin_test()` spatial null
  models
- **TemplateFlow integration**: Access standardized templates via the
  Python TemplateFlow API
- **Visualization**: Publication-quality surface figures with
  `plot_brain()` / `plot_brain_grid()`, perceptually-optimised ROI
  palettes, the ggseg ecosystem, and an interactive `cluster_explorer()`
  Shiny app
- **Provenance**: Every atlas carries structured space, family, and
  source metadata (`atlas_ref()`, `atlas_provenance()`)

## Installation

You can install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("bbuchsbaum/neuroatlas")
```

### TemplateFlow Setup

For TemplateFlow functionality, you’ll need Python with the templateflow
package:

``` r
# After installing neuroatlas:
neuroatlas::install_templateflow()
```

## Quick Start

``` r
library(neuroatlas)

# Get a Schaefer atlas (200 parcels, 7 networks)
schaefer <- get_schaefer_atlas(parcels = 200, networks = 7)
print(schaefer)

# Extract specific ROIs
roi <- get_roi(schaefer, "DefaultA")

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

`neuroatlas` now includes perceptually-optimised palettes for atlas
ROIs. For instance, you can generate a slice-aware palette for the
Schaefer 200×7 atlas and feed it directly into `ggseg`:

``` r
library(neuroatlas)
library(dplyr)
library(ggseg)

schaefer <- get_schaefer_atlas(parcels = 200, networks = 7)
meta <- roi_metadata(schaefer)

pal <- roi_colors_maximin_view(
  meta,
  hemi_col = "hemi",
  network_col = "network",
  pair_col = "pair_id",
  seed = 1
)

ggseg_schaefer(parcels = 200, networks = 7) +
  geom_sf(aes(fill = pal$color[match(label, pal$roi)])) +
  scale_fill_identity()
```

## Available Atlases

| Atlas | Function | Description |
|----|----|----|
| Schaefer | `get_schaefer_atlas()` | Cortical parcellations (100-1000 regions, 7 or 17 networks); surface via `get_schaefer_surfatlas()` |
| Brainnetome | `get_brainnetome_atlas()` | 246-region connectional atlas with Yeo network and cytoarchitectonic metadata |
| Glasser | `get_glasser_atlas()` | 360-region multi-modal cortical parcellation (surface via `glasser_surf()`) |
| Harvard-Oxford | `get_harvard_oxford_atlas()` | Cortical/subcortical structural atlases via TemplateFlow or FSL |
| Julich-Brain | `get_julich_brain_atlas()` | FSL Julich-Brain cytoarchitectonic atlas |
| ASEG | `get_aseg_atlas()` | FreeSurfer subcortical segmentation |
| Subcortical | `get_subcortical_atlas()` | Harmonized thalamus, cerebellum, and subcortex atlases (AtlasPack/TemplateFlow) |
| Olsen MTL | `get_olsen_mtl()` | Medial temporal lobe atlas with hippocampal subfields |
| Wang (2015) | `get_wang_atlas()` | Probabilistic visual topography on `fsaverage` (25 areas/hemi); probability volumes via `get_wang_prob_atlas()` |
| visfAtlas | `get_visfatlas()` | Probabilistic functional atlas of occipito-temporal visual cortex (33 regions) |
| Visual V1-V5 | `get_visual_atlas()` | Cytoarchitectonic early visual areas extracted from Julich-Brain |

## Documentation

- [Getting
  Started](https://bbuchsbaum.github.io/neuroatlas/articles/neuroatlas-overview.html) -
  Introduction and basic usage
- [Atlas Visualization with Optimal
  Colours](https://bbuchsbaum.github.io/neuroatlas/articles/atlas-visualization.html) -
  Perceptually-optimised ROI palettes
- [Surface Panel
  Figures](https://bbuchsbaum.github.io/neuroatlas/articles/surface-panels.html) -
  Static panel composition with `plot_brain()` and `plot_brain_grid()`
- [Surface
  Templates](https://bbuchsbaum.github.io/neuroatlas/articles/surface-templates.html) -
  Geometry vs. data on surface meshes
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

<!-- albersdown:theme-note:start -->

## Albers theme

This package uses the albersdown theme. Existing vignette theme hooks
are replaced so `albers.css` and local `albers.js` render consistently
on CRAN and GitHub Pages. The palette family is provided via
`params$family` (default ‘red’). The pkgdown site uses
`template: { package: albersdown }`. <!-- albersdown:theme-note:end -->

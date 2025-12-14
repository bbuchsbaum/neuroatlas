
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

- **Multiple Atlas Support**: Access Schaefer (100-1000 parcels),
  Glasser (360 regions), FreeSurfer ASEG, and Olsen MTL atlases
- **Flexible Resampling**: Transform atlases to different spaces and
  resolutions
- **ROI Analysis**: Extract and analyze specific regions of interest
  with `get_roi()`, `map_atlas()`, and `reduce_atlas()`
- **Surface & Volume**: Work with both volumetric and surface-based
  parcellations
- **TemplateFlow Integration**: Access standardized templates via the
  Python TemplateFlow API
- **Visualization**: Integration with the ggseg ecosystem for brain
  visualization

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
| Schaefer | `get_schaefer_atlas()` | Cortical parcellations (100-1000 regions, 7 or 17 networks) |
| Glasser | `get_glasser_atlas()` | 360-region multi-modal cortical parcellation |
| ASEG | `get_aseg_atlas()` | FreeSurfer subcortical segmentation |
| Olsen MTL | `get_olsen_mtl()` | Medial temporal lobe atlas with hippocampal subfields |

## Documentation

- [Getting
  Started](https://bbuchsbaum.github.io/neuroatlas/articles/neuroatlas-overview.html) -
  Introduction and basic usage
- [Working with
  TemplateFlow](https://bbuchsbaum.github.io/neuroatlas/articles/working-with-templateflow.html) -
  Template access and management
- [Surface
  Parcellations](https://bbuchsbaum.github.io/neuroatlas/articles/surface-parcellations.html) -
  Surface-based atlas operations
- [Function
  Reference](https://bbuchsbaum.github.io/neuroatlas/reference/index.html) -
  Complete API documentation

## Related Packages

- [neuroim2](https://github.com/bbuchsbaum/neuroim2) - Core neuroimaging
  data structures
- [neurosurf](https://github.com/bbuchsbaum/neurosurf) - Surface-based
  operations
- [ggseg](https://github.com/ggseg/ggseg) - Brain visualization

## License

MIT © Bradley Buchsbaum

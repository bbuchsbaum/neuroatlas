# neuroatlas ![](reference/figures/logo.png)

**neuroatlas** provides a unified interface for working with
neuroimaging atlases and parcellations in R. Whether you’re conducting
ROI-based analyses, visualizing brain data, or integrating different
parcellation schemes, neuroatlas streamlines these tasks with
consistent, user-friendly functions.

## Features

- **Many built-in atlases**: Schaefer (100-1000 parcels), Brainnetome
  (246 regions), Glasser (360 regions), HCPex (426 regions),
  Harvard-Oxford, Julich-Brain, FreeSurfer ASEG, harmonized
  TemplateFlow/AtlasPack subcortical atlases, Olsen MTL, and
  probabilistic visual-cortex atlases (Wang 2015, visfAtlas,
  cytoarchitectonic V1-V5)
- **Surface & volume**: Work with both volumetric and surface-based
  parcellations through one consistent interface
- **Atlas discovery**: Enumerate built-ins with
  [`list_atlases()`](https://bbuchsbaum.github.io/neuroatlas/reference/list_atlases.md)
  and load any of them by name with
  [`get_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_atlas.md)
- **ROI analysis**: Extract and summarise regions with
  [`get_roi()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_roi.md),
  [`map_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/map_atlas.md),
  [`reduce_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/reduce_atlas.md),
  and
  [`batch_reduce()`](https://bbuchsbaum.github.io/neuroatlas/reference/batch_reduce.md)
- **Atlas operations**: Combine and reshape parcellations with
  [`merge_atlases()`](https://bbuchsbaum.github.io/neuroatlas/reference/merge_atlases.md),
  [`filter_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/filter_atlas.md),
  [`dilate_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/dilate_atlas.md),
  [`atlas_overlap()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_overlap.md),
  and resampling across spaces/resolutions
- **Template transforms**: Resolve verified image-space transforms,
  resample labels or continuous/probability volumes, and keep artifact
  and interpolation receipts
- **Spatial queries**: Look up parcels by world, voxel, or MNI
  coordinate with
  [`query_point()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_point.md),
  [`query_coord()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_coord.md),
  and
  [`query_vox()`](https://bbuchsbaum.github.io/neuroatlas/reference/query_coord.md)
- **Network & graph tools**:
  [`atlas_connectivity()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_connectivity.md),
  [`atlas_graph()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_graph.md)
  /
  [`as_igraph()`](https://bbuchsbaum.github.io/neuroatlas/reference/as_igraph.md),
  [`atlas_hierarchy()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_hierarchy.md),
  and
  [`spin_test()`](https://bbuchsbaum.github.io/neuroatlas/reference/spin_test.md)
  spatial null models
- **TemplateFlow integration**: Access standardized templates through
  the pure-R `templateflow` backend
- **Visualization**: Publication-quality surface figures with
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  /
  [`plot_brain_grid()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain_grid.md),
  sulcal shading from topology-checked white-surface curvature
  ([`surface_anatomy()`](https://bbuchsbaum.github.io/neuroatlas/reference/surface_anatomy.md)),
  perceptually-optimised ROI palettes, the ggseg ecosystem, and an
  interactive
  [`cluster_explorer()`](https://bbuchsbaum.github.io/neuroatlas/reference/cluster_explorer.md)
  Shiny app
- **Metadata**: Atlases and loaded templates carry identity, spatial
  geometry, citations, source artifacts, and processing history
  ([`atlas_metadata()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_metadata.md),
  [`template_metadata()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_metadata.md)).

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

### HCPex: cortex and subcortex

[HCPex](https://github.com/wayalan/HCPex) adds 66 subcortical regions to
the 360 cortical regions of HCP-MMP1.0. Load the upstream v1.1 volume at
1 or 2 mm:

``` r

hcp <- get_atlas("hcpex", resolution = 2)
get_roi(hcp, label = "V1_L")
subcortex <- filter_atlas(hcp, division == "subcortical")
roi_metadata(subcortex)  # Abbreviations, full names, hemisphere, and colors
atlas_citations(hcp)
```

The source declares MNI152NLin2009cAsym template space. HCPex uses its
own region ordering; reorder matrices from original HCP-MMP1.0 order
before mapping them to this atlas. Downloads are cached and checked
against pinned checksums. The upstream atlas data are licensed under
GPL-3.0.

### Identify and cite a resource

``` r

aseg <- get_aseg_atlas()
atlas_metadata(aseg)       # Identity, current geometry, source, and modifications
atlas_citations(aseg)      # R bibliography entries, ready for toBibtex()
atlas_artifacts(aseg)      # Original files, license information, and checksums
atlas_history(aseg)        # Recorded operations and their parameters
```

Metadata stay attached when objects are saved. Source-file geometry and
current object geometry are recorded separately, and unknown provenance
is explicit. See [Identify, cite, and trace an
atlas](https://bbuchsbaum.github.io/neuroatlas/vignettes/resource-metadata.Rmd).

### Move an atlas to another template

[`transform_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/transform_atlas.md)
preserves region IDs and samples labels on the target grid. Check
[`space_transform_manifest()`](https://bbuchsbaum.github.io/neuroatlas/reference/space_transform_manifest.md)
for available routes. The transform engine is an optional dependency;
install the pinned revision used for qualification:

``` r

pak::pak(c("hdf5r",
  "bbuchsbaum/neurotransform@9e7550d45d6d6b06155b27717057f4137aaf666e"))
```

``` r

schaefer <- get_schaefer_atlas(parcels = 200, networks = 7, resolution = 2)
aligned <- transform_atlas(
  schaefer, "MNI152NLin2009cAsym", resolution = 2, provider = "neuroatlas"
)
attr(aligned, "neuroatlas_transform")$lost_label_ids
```

Both MNI152NLin6Asym / MNI152NLin2009cAsym directions are available from
the [qualified artifact
release](https://github.com/bbuchsbaum/neuroatlas/releases/tag/transform-artifacts-v1).
Qualification covers 1 mm and 2 mm target grids. Each transform is about
86 MiB, downloaded once and checked against its pinned SHA-256 digest.
Subsequent calls can use `offline = TRUE` with a cached transform and an
explicit target grid. For scalar or probability volumes, use
[`get_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_template_transform.md)
and
[`apply_template_transform()`](https://bbuchsbaum.github.io/neuroatlas/reference/apply_template_transform.md)
with explicit sampling semantics. Probability channels are interpolated
independently without renormalization. Template-derived artifacts have
separate [distribution
conditions](https://github.com/bbuchsbaum/neuroatlas/releases/download/transform-artifacts-v1/LICENSES.md).

An image-space transform does not establish correspondence between
different parcellations. Use
[`atlas_overlap()`](https://bbuchsbaum.github.io/neuroatlas/reference/atlas_overlap.md)
after alignment to compare their regions. See [Verified template
transforms](https://bbuchsbaum.github.io/neuroatlas/vignettes/template-transforms.Rmd).

## Palette demos

`neuroatlas` includes perceptually-optimised palettes for atlas ROIs.
For instance, you can generate a slice-aware palette for the Schaefer
200×7 atlas and feed it directly into
[`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md):

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

### Anatomical shading

CPU renders of inflated surfaces shade sulci and gyri from mean
curvature of the matching white mesh. Shading is applied only when the
two meshes share a topology. Use
[`surface_anatomy()`](https://bbuchsbaum.github.io/neuroatlas/reference/surface_anatomy.md)
to inspect the metric and its provenance, or to supply your own
per-vertex sulcal depth. Tune the underlay with the `anatomy_*`
arguments of
[`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md):

``` r

anat <- surface_anatomy(schaefer_surface, hemi = "lh")
anat$provenance$source  # "computed_mean_curvature"

plot_brain(
  schaefer_surface,
  interactive = FALSE,
  anatomy_range = c(0.65, 0.92)
)
```

## Available Atlases

| Atlas | Function | Description |
|----|----|----|
| Schaefer | [`get_schaefer_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_schaefer_atlas.md) | Cortical parcellations (100-1000 regions, 7 or 17 networks); surface via [`get_schaefer_surfatlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_schaefer_surfatlas.md) |
| Brainnetome | [`get_brainnetome_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_brainnetome_atlas.md) | 246-region connectional atlas with Yeo network and cytoarchitectonic metadata |
| Glasser | [`get_glasser_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_glasser_atlas.md) | 360-region multi-modal cortical parcellation (surface via [`glasser_surf()`](https://bbuchsbaum.github.io/neuroatlas/reference/glasser_surf.md)) |
| HCPex | [`get_hcpex_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_hcpex_atlas.md) | 360 cortical and 66 subcortical regions, native 1/2 mm volumes in MNI152NLin2009cAsym |
| Harvard-Oxford | [`get_harvard_oxford_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_harvard_oxford_atlas.md) | Cortical/subcortical structural atlases via TemplateFlow or FSL |
| Julich-Brain | [`get_julich_brain_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_julich_brain_atlas.md) | FSL Julich-Brain cytoarchitectonic atlas |
| ASEG | [`get_aseg_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_aseg_atlas.md) | FreeSurfer subcortical segmentation |
| Subcortical | [`get_subcortical_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_subcortical_atlas.md) | Harmonized thalamus, cerebellum, and subcortex atlases (AtlasPack/TemplateFlow) |
| Olsen MTL | [`get_olsen_mtl()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_olsen_mtl.md) | Medial temporal lobe atlas with hippocampal subfields |
| Wang (2015) | [`get_wang_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_wang_atlas.md) | Probabilistic visual topography on `fsaverage` (25 areas/hemi); probability volumes via [`get_wang_prob_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_wang_prob_atlas.md) |
| visfAtlas | [`get_visfatlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_visfatlas.md) | Probabilistic functional atlas of occipito-temporal visual cortex (33 regions) |
| Visual V1-V5 | [`get_visual_atlas()`](https://bbuchsbaum.github.io/neuroatlas/reference/get_visual_atlas.md) | Cytoarchitectonic early visual areas extracted from Julich-Brain |

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
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  and
  [`plot_brain_grid()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain_grid.md)
- [Surface
  Templates](https://bbuchsbaum.github.io/neuroatlas/articles/surface-templates.html) -
  Geometry vs. data on surface meshes
- [Surface
  Parcellations](https://bbuchsbaum.github.io/neuroatlas/articles/surface-parcellations.html) -
  Surface-based atlas operations
- [Verified Template
  Transforms](https://bbuchsbaum.github.io/neuroatlas/articles/template-transforms.html) -
  Align volumes and atlases on explicit grids
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
on CRAN and GitHub Pages. The palette family is provided via
`params$family` (default ‘red’). The pkgdown site uses
`template: { package: albersdown }`.

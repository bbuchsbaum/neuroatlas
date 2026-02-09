<!-- Parent: ../AGENTS.md -->
<!-- Generated: 2026-02-08 | Updated: 2026-02-08 -->

# R/

## Purpose
All R source code for the neuroatlas package. Contains S3 class definitions, generic functions, atlas loaders, data reduction methods, visualization helpers, and TemplateFlow integration.

## Key Files

### Core Framework

| File | Description |
|------|-------------|
| `all_generic.R` | S3 generic definitions: `get_roi`, `map_atlas`, `reduce_atlas`, `reduce_atlas_vec`, `sub_atlas` |
| `atlas.R` | Base `atlas` class: `print.atlas`, `merge_atlases`, `get_roi.atlas`, `sub_atlas.atlas`, `reduce_atlas.atlas` |
| `atlas_utils.R` | Internal helper `.get_atlas_volume()` for extracting volume from atlas objects |
| `atlas_methods.R` | Additional atlas method implementations |
| `roi_metadata.R` | `roi_metadata()`, `roi_attributes()`, `filter_atlas()` generics/methods plus internal `.subset_atlas()`, `.subset_atlas_data()`, `.build_roi_metadata()` |

### Atlas Loaders

| File | Description |
|------|-------------|
| `schaefer.R` | `get_schaefer_atlas()` (volumetric) and `get_schaefer_surfatlas()` (surface) — Schaefer cortical parcellations (100-1000 regions, 7/17 networks) |
| `glasser.R` | `get_glasser_atlas()` (volumetric) and `glasser_surf()` (surface) — Glasser 360-region multi-modal parcellation |
| `aseg_subcort.R` | `get_aseg_atlas()` — FreeSurfer subcortical segmentation (17 regions) |
| `olsen_mtl.R` | `get_olsen_mtl()` and `get_hipp_atlas()` — Medial temporal lobe and hippocampal subfield atlases |
| `subcortical_atlases.R` | `get_subcortical_atlas()` — Unified loader for subcortical atlas variants |

### Data Reduction & Spatial Operations

| File | Description |
|------|-------------|
| `reduce_atlas_vec.R` | `reduce_atlas_vec.atlas()` — Summarize 4D data by atlas parcels into `ClusteredNeuroVec` |
| `dilate_parcels.R` | `dilate_atlas()` — Expand parcel boundaries into unassigned mask voxels |
| `coordinate_spaces.R` | MNI152/MNI305 coordinate transforms, space resolution helpers |

### Visualization & Colors

| File | Description |
|------|-------------|
| `print-plot-methods.R` | `print` and `plot` methods for atlas classes |
| `chart.R` | Chart/plotting utilities |
| `ggschaefer.R` | ggseg-based Schaefer atlas visualization |
| `roi_colors.R` | Color generation algorithms for atlas regions |

### Templates & Surfaces

| File | Description |
|------|-------------|
| `template_flow.R` | TemplateFlow Python bridge via reticulate; `get_template()`, `install_templateflow()` |
| `fsaverage.R` | fsaverage surface template loading |
| `data-schaefer.R` | Lazy data documentation for bundled Schaefer parcellation data |

### Package Infrastructure

| File | Description |
|------|-------------|
| `neuroatlas-package.R` | Package-level roxygen2 documentation |
| `zzz.R` | `.onLoad` / `.onAttach` hooks |

## For AI Agents

### Working In This Directory
- One main topic per file; keep file names lowercase and descriptive
- All exported functions need roxygen2 `#'` documentation with `@export`
- After adding/changing exports or `@importFrom` tags, run `devtools::document()`
- Internal helpers should use `@keywords internal` and `@noRd` (no Rd file generated)
- Prefer editing existing files over creating new ones

### Key Patterns
- Atlas constructors return `list(...)` with `class(ret) <- c("specific", "atlas")`
- Surface atlases use `class(ret) <- c("specific", "surfatlas", "atlas")`
- Subsetting flows through `.subset_atlas()` → `.subset_atlas_data()` (in `roi_metadata.R`)
- `ClusteredNeuroVol`-backed atlases require contiguous 1:K cluster IDs for `ClusteredNeuroVec` construction
- Use `methods::is(x, "ClassName")` for S4 class checks (not `inherits()`)

### Common Pitfalls
- `as.matrix()` on S4 objects may not dispatch in test environments — use `neuroim2::values()` instead
- `%in%` on `ClusteredNeuroVol` objects may fail — convert to array first or use `@clusters`
- TemplateFlow functions require Python/reticulate setup — always guard with `tryCatch` or `skip_on_cran()`

## Dependencies

### Internal
- `atlas_utils.R` is used by `atlas.R` and `reduce_atlas_vec.R`
- `roi_metadata.R` provides `.subset_atlas()` used by `atlas.R` (`sub_atlas`, `filter_atlas`)
- Atlas loaders (`schaefer.R`, `glasser.R`, etc.) produce objects consumed by generics in `all_generic.R`

### External
- `neuroim2` — NeuroVol, NeuroSpace, ClusteredNeuroVol, ClusteredNeuroVec, ROIVol
- `neurosurf` — `read_freesurfer_annot()` for surface atlas loading
- `reticulate` — Python interop for TemplateFlow
- `tibble`, `dplyr`, `rlang` — Data manipulation and tidy evaluation

<!-- MANUAL: -->

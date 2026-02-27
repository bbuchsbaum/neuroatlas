# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

### Development Commands

- **Build package**:
  [`devtools::build()`](https://devtools.r-lib.org/reference/build.html)
  or `R CMD build .`
- **Check package**:
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  or `R CMD check --as-cran`
- **Run tests**:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  or
  [`testthat::test_local()`](https://testthat.r-lib.org/reference/test_package.html)
- **Run specific test**:
  `testthat::test_file("tests/testthat/test-dilation.R")`
- **Install package**:
  [`devtools::install()`](https://devtools.r-lib.org/reference/install.html)
  or `R CMD INSTALL`
- **Load for development**:
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
- **Document**:
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  to update NAMESPACE and man files from roxygen2 comments
- **Build README**:
  [`devtools::build_readme()`](https://devtools.r-lib.org/reference/build_rmd.html)
  to render README.Rmd to README.md
- **Build vignettes**:
  [`devtools::build_vignettes()`](https://devtools.r-lib.org/reference/build_vignettes.html)
- **Build pkgdown site**:
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)

## Architecture Overview

This R package provides a unified framework for neuroimaging atlases and
parcellations.

### Core S3 Object System

All atlases inherit from base `atlas` class defined in `atlas.R`.
Required fields for any atlas object: - `atlas`: The underlying
NeuroVol/ClusteredNeuroVol containing parcel labels - `labels`: Region
label names - `ids`: Numeric region IDs - `name`: Atlas name string -
`hemi`: Hemisphere designation (“left”/“right”/NA)

### Generic Functions (`all_generic.R`)

- `get_roi(x, label, id, hemi)`: Extract specific regions from any atlas
- `map_atlas(x, vals, thresh, ...)`: Map values to atlas regions
- `reduce_atlas(atlas, data_vol, stat_func, ..., format)`: Summarize
  data within atlas regions

### Atlas Types

| File | Atlas | Description |
|----|----|----|
| `schaefer.R` | Schaefer | Cortical parcellations (100-1000 regions, 7/17 networks), volume + surface |
| `glasser.R` | Glasser | 360-region multi-modal cortical parcellation |
| `aseg_subcort.R` | ASEG | FreeSurfer subcortical segmentation |
| `olsen_mtl.R` | Olsen MTL | Medial temporal lobe with hippocampal subfields |

### TemplateFlow Integration (`template_flow.R`)

Python bridge via reticulate to TemplateFlow API.

**Primary function**:
`get_template(space, variant, modality, resolution, ...)` - `space`:
Template identifier (e.g., “MNI152NLin2009cAsym”) - `variant`: “brain”,
“head”, “mask”, “probseg”, “dseg” - `modality`: “T1w”, “T2w” - Returns
NeuroVol objects or paths (with `path_only=TRUE`)

**Troubleshooting TemplateFlow**:

``` r
# Check Python availability
reticulate::py_available()

# Install templateflow (creates persistent environment)
neuroatlas::install_templateflow()

# If in ephemeral environment (quick fix)
reticulate::py_install(c("scipy", "templateflow"))

# Check connectivity
neuroatlas::.check_templateflow_connectivity()

# View/clear cache
show_templateflow_cache_path()
clear_templateflow_cache()
```

### Key Dependencies

- **neuroim2**: Core neuroimaging data structures (NeuroVol, NeuroSpace,
  ClusteredNeuroVol)
- **neurosurf**: Surface-based operations
- **reticulate**: Python interoperability
- **ggseg/ggsegSchaefer/ggsegGlasser**: Brain visualization

## Testing

Tests in `tests/testthat/` use testthat edition 3. Key patterns: - Use
`skip_on_cran()` for network-dependent or slow tests - Test files mirror
R source topics (e.g., `test-dilation.R` tests `dilate_parcels.R`)

## Style Guidelines

- Follow tidyverse conventions: 2-space indent, ~80 char lines
- Use `snake_case` for functions; `UpperCamelCase` for S3/S4 classes
- Document exports with roxygen2 (`#'`); run
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  after changes
- Use `assertthat` for input validation
- Check optional packages with
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) before use

## CRAN Compliance Notes

- No non-ASCII characters in code or data
- Examples must run in \< 5 seconds (use `\dontrun{}` or `\donttest{}`
  for slow examples)
- Package data size ~23MB (acceptable for neuroimaging atlas package)

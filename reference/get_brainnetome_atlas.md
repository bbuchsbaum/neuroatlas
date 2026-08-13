# Load Brainnetome 246-Region Atlas

Downloads and loads the Brainnetome Atlas 246-region MNI152 1 mm
labelmap. Files are cached under the neuroatlas user cache directory
rather than bundled with the package.

## Usage

``` r
get_brainnetome_atlas(outspace = NULL, smooth = FALSE, use_cache = TRUE)
```

## Source

<https://atlas.brainnetome.org/download.html>

## Arguments

- outspace:

  Optional `NeuroSpace` object or TemplateFlow-style outspace
  descriptor. If supplied, the atlas is resampled to that space.

- smooth:

  Logical. Whether to smooth parcel boundaries when resampling.

- use_cache:

  Logical. Use cached Brainnetome files when available.

## Value

A list with classes `c("brainnetome", "volatlas", "atlas")`.

## Details

The Brainnetome download page describes non-commercial use and
attribution terms. This loader keeps the data outside the package source
and records the upstream assets in
[`atlas_artifacts()`](atlas_artifacts.md).

## References

Fan, L. et al. (2016). The Human Brainnetome Atlas: A New Brain Atlas
Based on Connectional Architecture. Cerebral Cortex, 26(8), 3508-3526.

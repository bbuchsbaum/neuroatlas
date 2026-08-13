# Brainnetome Atlas Label Table

Downloads (if needed) and returns the Brainnetome 246-region label table
used by [`get_brainnetome_atlas()`](get_brainnetome_atlas.md).

## Usage

``` r
brainnetome_labels(use_cache = TRUE)
```

## Source

<https://atlas.brainnetome.org/download.html>

## Arguments

- use_cache:

  Logical. Use cached Brainnetome files when available.

## Value

A tibble with one row per parcel and columns for parcel id, label,
hemisphere, lobe/gyrus, Yeo network membership, RGB colour, and
cytoarchitectonic description.

## Details

Brainnetome assets are downloaded on demand and cached locally. They are
not bundled with neuroatlas; use is governed by the legal agreement on
the Brainnetome download page.

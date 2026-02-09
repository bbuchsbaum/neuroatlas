# Extract Hippocampal Parcellation

Creates a hippocampus-specific atlas from the Olsen MTL atlas, with
optional anterior-posterior subdivisions.

## Usage

``` r
get_hipp_atlas(outspace = NULL, apsections = 1)
```

## Arguments

- outspace:

  Optional `NeuroSpace` object for resampling

- apsections:

  Integer specifying number of anterior-posterior divisions. Default: 1
  (no subdivision)

## Value

A list with class c("hippocampus", "atlas") containing:

- name:

  Character string "hippocampus"

- atlas:

  NeuroVol object with hippocampal parcellation

- ids:

  Integer vector of region IDs

- labels:

  Character vector of region labels

- hemi:

  Character vector of hemisphere designations

- cmap:

  Matrix of RGB colors for visualization

- orig_labels:

  Full labels including hemisphere information

## Details

This function extracts hippocampal regions from the full MTL atlas and
can subdivide them into anterior-posterior segments. The resulting atlas
maintains bilateral organization and can be used for targeted
hippocampal analyses.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic hippocampal atlas
hipp <- get_hipp_atlas()

# With anterior-posterior subdivisions
hipp_ap <- get_hipp_atlas(apsections = 3)
} # }
```

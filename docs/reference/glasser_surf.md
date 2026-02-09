# Glasser Surface Atlas (fsaverage)

Load the Glasser HCP-MMP1.0 cortical parcellation projected to the
FreeSurfer `fsaverage` surface, as distributed by Kathryn Mills (see
Figshare dataset "HCP-MMP1.0 projected on fsaverage"). The result is a
pair of neurosurf `LabeledNeuroSurface` objects plus atlas metadata.

## Usage

``` r
glasser_surf(
  space = "fsaverage",
  surf = c("pial", "white", "inflated", "midthickness"),
  use_cache = TRUE
)
```

## Arguments

- space:

  Surface space / mesh template. Only `"fsaverage"` is supported at
  present.

- surf:

  Surface type. One of `"pial"`, `"white"`, `"inflated"`, or
  `"midthickness"`.

- use_cache:

  Logical. Whether to cache downloaded annotation files in the
  neuroatlas cache directory. Default: `TRUE`.

## Value

A list with classes `c("glasser_surf","surfatlas","atlas")` containing:

- `lh_atlas`, `rh_atlas`: `LabeledNeuroSurface` objects for left and
  right hemispheres.

- `surf_type`: requested surface type.

- `surface_space`: surface template space ("fsaverage").

- `ids`, `labels`, `orig_labels`, `hemi`, `cmap`: atlas metadata.

## Details

This function uses:

- fsaverage surface geometry from TemplateFlow via
  [`get_surface_template`](get_template.md)

- fsaverage `.annot` files from the Mills Figshare distribution
  (`lh.HCP-MMP1.annot`, `rh.HCP-MMP1.annot`)

Currently only the `"fsaverage"` surface space is supported.

## Examples

``` r
if (FALSE) { # \dontrun{
# Glasser MMP1.0 on fsaverage pial surface
atl <- glasser_surf(space = "fsaverage", surf = "pial")
} # }
```

# Schaefer Surface Atlas

Load the Schaefer2018 cortical parcellation as a surface atlas,
returning neurosurf `LabeledNeuroSurface` objects plus standard atlas
metadata. By default this uses the packaged fsaverage6 geometry; other
FreeSurfer surface spaces use TemplateFlow for mesh geometry.

## Usage

``` r
schaefer_surf(
  parcels = c(100, 200, 300, 400, 500, 600, 800, 1000),
  networks = c(7, 17),
  space = c("fsaverage6", "fsaverage", "fsaverage5"),
  surf = c("inflated", "white", "pial"),
  use_cache = TRUE
)
```

## Arguments

- parcels:

  Number of parcels. Can be numeric or character; valid values are 100,
  200, 300, 400, 500, 600, 800, 1000.

- networks:

  Number of networks. Can be numeric or character; valid values are 7 or
  17.

- space:

  Surface space / mesh template. One of `"fsaverage6"` (default),
  `"fsaverage"`, or `"fsaverage5"`. Currently only `"fsaverage6"` uses
  packaged geometry; other spaces require a working TemplateFlow setup.

- surf:

  Surface type. One of `"inflated"`, `"white"`, or `"pial"`.

- use_cache:

  Logical. Passed to `schaefer_metainfo()` for label metadata caching.

## Value

A list with classes `c("schaefer","surfatlas","atlas")` containing:

- `lh_atlas`, `rh_atlas`: `LabeledNeuroSurface` objects for left and
  right hemispheres.

- `surf_type`: requested surface type.

- `surface_space`: Schaefer surface space (e.g. fsaverage6).

- `ids`, `labels`, `orig_labels`, `network`, `hemi`, `cmap`: atlas
  metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# Schaefer 200 parcels, 17 networks on fsaverage6 inflated surface
atl <- schaefer_surf(parcels = 200, networks = 17,
                     space = "fsaverage6", surf = "inflated")
} # }
```

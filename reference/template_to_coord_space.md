# Get Coordinate Space for Any Template

Determine which standard coordinate space a template's data are defined
in. Handles both surface templates (e.g., fsaverage) and volumetric
templates (e.g., MNI152NLin6Asym).

## Usage

``` r
template_to_coord_space(template_id)
```

## Arguments

- template_id:

  Character string identifying the template. Examples: "fsaverage",
  "fsaverage6", "fsLR_32k", "MNI152NLin6Asym", "MNI152NLin2009cAsym",
  "MNI152", "MNI305".

## Value

Character string indicating the coordinate space (\`"MNI305"\`,
\`"MNI152"\`, or \`"Unknown"\`).

## Details

This function normalizes the input using the same alias resolution as
the space transform registry, then maps it to a coordinate space:

- MNI305:

  fsaverage, fsaverage5, fsaverage6, MNI305

- MNI152:

  fsLR_32k, MNI152, MNI152NLin6Asym, MNI152NLin2009cAsym

## See also

\[get_surface_coordinate_space()\] for the surface-only version,
\[needs_coord_transform()\] and \[needs_template_warp()\] for transform
checks.

## Examples

``` r
template_to_coord_space("fsaverage")            # "MNI305"
#> [1] "MNI305"
template_to_coord_space("MNI152NLin6Asym")      # "MNI152"
#> [1] "MNI152"
template_to_coord_space("MNI152NLin2009cAsym")   # "MNI152"
#> [1] "MNI152"
template_to_coord_space("fsLR_32k")              # "MNI152"
#> [1] "MNI152"
```

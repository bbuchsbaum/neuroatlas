# Get Coordinate Space for a Surface Template

Determine which standard coordinate space a surface template's vertices
are defined in.

## Usage

``` r
get_surface_coordinate_space(template_id)
```

## Arguments

- template_id:

  Character string identifying the surface template. Examples:
  "fsaverage", "fsaverage5", "fsaverage6", "fsLR".

## Value

Character string indicating the coordinate space:

- `"MNI305"` for fsaverage variants (fsaverage, fsaverage5, fsaverage6)

- `"MNI152"` for fsLR (HCP template)

- `"Unknown"` for unrecognized templates

## Details

FreeSurfer's fsaverage surfaces were created using Talairach
registration, which targets MNI305 space. All fsaverage density variants
(fsaverage, fsaverage5, fsaverage6) share this coordinate system.

The HCP's fsLR template was designed to align with MNI152 space, so no
transform is needed when working with MNI152 volumetric data.

## See also

[`transform_coords`](transform_coords.md),
[`get_space_transform`](get_space_transform.md)

## Examples

``` r
get_surface_coordinate_space("fsaverage")   # "MNI305"
#> [1] "MNI305"
get_surface_coordinate_space("fsaverage6")  # "MNI305"
#> [1] "MNI305"
get_surface_coordinate_space("fsLR")        # "MNI152"
#> [1] "MNI152"
```

# Check if Transform is Needed Between Spaces

Convenience function to check whether a coordinate transform is required
when working with a surface template and volumetric data in a given
space.

## Usage

``` r
needs_transform(surface_template, volume_space)
```

## Arguments

- surface_template:

  Character string identifying the surface template (e.g., "fsaverage",
  "fsLR").

- volume_space:

  Character string identifying the volumetric data's coordinate space
  (e.g., "MNI152", "MNI305").

## Value

Logical. `TRUE` if a transform is needed, `FALSE` if the spaces match.

## Examples

``` r
# fsaverage + MNI152 volume: transform needed
needs_transform("fsaverage", "MNI152")  # TRUE
#> [1] TRUE

# fsLR + MNI152 volume: no transform needed
needs_transform("fsLR", "MNI152")  # FALSE
#> [1] FALSE

# fsaverage + MNI305 volume: no transform needed
needs_transform("fsaverage", "MNI305")  # FALSE
#> [1] FALSE
```

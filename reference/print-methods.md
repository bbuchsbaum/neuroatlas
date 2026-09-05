# Print Methods for neuroatlas Objects

Print methods for various atlas objects in the neuroatlas package

## Usage

``` r
# S3 method for class 'atlas'
print(x, ...)

# S3 method for class 'atlas_provenance'
print(x, ...)

# S3 method for class 'glasser'
print(x, ...)

# S3 method for class 'schaefer'
print(x, ...)
```

## Arguments

- x:

  An atlas object (atlas, glasser, schaefer, etc.)

- ...:

  Additional arguments passed to print

## Value

Invisibly returns `x`.

The object is returned invisibly

## Examples

``` r
atlas <- get_aseg_atlas()
print(atlas)
#> <atlas metadata> ASEG
#>   Description:   Bundled standard-space FreeSurfer subcortical labels.
#>   Version:       not recorded
#>   Content:       labels, volume
#>   Regions:       17
#>   Template:      MNI152_unspecified
#>   Coord. space:  MNI152
#>   Space basis:   inferred
#>   Voxel size:    1 x 1 x 1 mm
#>   Source:        bundled_extdata
#>   License:       not recorded
#>   Citation:      [atlas] Bruce Fischl et al. (2002); doi:10.1016/S0896-6273(02)00569-X
#>   Modifications: none recorded
```

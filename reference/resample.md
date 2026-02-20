# Resample Volume to New Space

Resamples a volume to a new space with optional smoothing of parcel
boundaries. This is particularly useful for atlas parcellations where
maintaining discrete labels is important.

## Usage

``` r
resample(
  vol,
  outspace,
  smooth = FALSE,
  interp = 0,
  radius = NULL,
  min_neighbors = 3
)
```

## Arguments

- vol:

  A NeuroVol object to be resampled

- outspace:

  A NeuroSpace object specifying the target space

- smooth:

  Logical. Whether to apply boundary smoothing after resampling.
  Default: FALSE

- interp:

  Integer. Interpolation method (0=nearest neighbor, 1=linear). Default:
  0

- radius:

  Numeric. Radius for smoothing neighborhood in voxels. If NULL, uses
  max(spacing)+0.5. Default: NULL

- min_neighbors:

  Integer. Minimum number of neighbors required for smoothing. Default:
  3

## Value

A resampled NeuroVol object in the new space

## Details

The resampling process:

- First performs nearest-neighbor interpolation to the new space

- Optionally smooths boundaries using a local majority voting scheme

- Preserves zeros in the mask (background)

## Examples

``` r
# \donttest{
atlas <- get_aseg_atlas()
vol <- atlas$atlas
new_space <- neuroim2::NeuroSpace(
  dim = dim(vol), spacing = neuroim2::spacing(vol)
)
resampled <- resample(vol, new_space)
#> Warning: Some original labels were lost during resampling
# }
```

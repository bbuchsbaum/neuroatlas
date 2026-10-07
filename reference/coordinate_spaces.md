# Coordinate Space Transforms for Neuroimaging Templates

Functions and constants for transforming coordinates between standard
neuroimaging coordinate spaces, particularly MNI305 (fsaverage) and
MNI152 (common fMRI template space).

## Value

This documentation topic describes legacy coordinate-family constants
and affine helpers. Use exact template identifiers for qualified
registration.

## Details

These legacy helpers classify broad coordinate families and apply a
fixed FreeSurfer affine. They do not establish template-specific
registration or cortical correspondence. Use exact MNI template
identifiers and qualified transforms for image resampling or population
cortical projection.

## Coordinate Spaces

- MNI305:

  FreeSurfer/Talairach space. Native space for fsaverage, fsaverage5,
  and fsaverage6 surfaces. Based on 305 subjects with linear
  registration to approximate Talairach space.

- MNI152:

  A broad coordinate-family label. MNI152NLin6Asym and
  MNI152NLin2009cAsym are distinct templates; this shorthand does not
  identify either variant or a registration between them.

## References

FreeSurfer CoordinateSystems documentation:
<https://surfer.nmr.mgh.harvard.edu/fswiki/CoordinateSystems>

Wu et al. (2018). Accurate nonlinear mapping between MNI volumetric and
FreeSurfer surface coordinate systems. Human Brain Mapping, 39(9),
3793-3808. [doi:10.1002/hbm.24213](https://doi.org/10.1002/hbm.24213)

## Examples

``` r
transform_coords(matrix(c(0, 0, 0), nrow = 1),
  from = "MNI305", to = "MNI152"
)
#>         [,1]   [,2]  [,3]
#> [1,] -0.0429 1.5496 1.184
```

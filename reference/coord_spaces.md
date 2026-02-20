# Standard Coordinate Space Identifiers

Character constants for standard neuroimaging coordinate spaces.

## Usage

``` r
coord_spaces
```

## Format

A named list with the following elements:

- MNI305:

  FreeSurfer/Talairach space (fsaverage native)

- MNI152:

  ICBM 2009c space (common fMRI template)

- SCANNER:

  Native scanner space (subject-specific)

- UNKNOWN:

  Unknown or unspecified space

## Examples

``` r
coord_spaces$MNI305
#> [1] "MNI305"
coord_spaces$MNI152
#> [1] "MNI152"
```

# Extract a region of interest (ROI) from an atlas

Extracts a specific region of interest from an atlas object based on
label, ID, and hemisphere information.

## Usage

``` r
get_roi(x, label = NULL, id = NULL, hemi = NULL)

# S3 method for class 'atlas'
get_roi(x, label = NULL, id = NULL, hemi = NULL)
```

## Arguments

- x:

  An atlas object

- label:

  Character string specifying the ROI label/name

- id:

  Numeric ID of the ROI in the atlas

- hemi:

  Character string specifying hemisphere ('left' or 'right')

## Value

Returns a subset of the atlas containing only the specified ROI

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the aseg atlas
atlas <- get_aseg_atlas()

# Extract the hippocampus ROI
roi <- get_roi(atlas, label = "Hippocampus")
} # }
```

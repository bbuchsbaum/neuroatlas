# Map values to an atlas

Maps a set of values to regions/parcels in an atlas object. This can be
used to visualize data (like statistics or measurements) across atlas
regions.

## Usage

``` r
map_atlas(x, vals, thresh, ...)

# S3 method for class 'atlas'
map_atlas(x, vals, thresh = NULL, pos = FALSE, ...)

# S3 method for class 'schaefer'
map_atlas(x, vals, thresh = NULL, pos = FALSE, ...)

# S3 method for class 'glasser'
map_atlas(x, vals, thresh = NULL, pos = FALSE, ...)
```

## Arguments

- x:

  An atlas object to map values onto

- vals:

  Numeric vector of values to map to atlas regions. Length should match
  the number of regions in the atlas

- thresh:

  Optional numeric vector of length 2 specifying (min, max) thresholds
  for the mapped values. Values outside this range will be clamped.

- ...:

  Additional arguments passed to methods

- pos:

  Logical. If \`TRUE\`, values are thresholded using raw values;
  otherwise the absolute values are used.

## Value

Returns the atlas object with mapped values

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the aseg atlas
atlas <- get_aseg_atlas()
vals <- rnorm(length(atlas$orig_labels))

# Map values with a threshold of -2 to 2
mapped <- map_atlas(atlas, vals, thresh = c(-2, 2))
} # }
```

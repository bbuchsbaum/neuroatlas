# Align a Parcel Value Column to an Atlas

Match a data-frame column to atlas parcels and return a numeric vector
in exact \`atlas\$ids\` order. Matching is strict by default: atlas and
data keys must be unique, data keys must all exist in the atlas, and
every atlas parcel must be represented unless \`allow_partial = TRUE\`.

## Usage

``` r
align_parcel_values(atlas, data, value, by = NULL, allow_partial = FALSE)
```

## Arguments

- atlas:

  An atlas object.

- data:

  A data frame, tibble, or \`parcel_data\` object containing parcel keys
  and values.

- value:

  A numeric value column, supplied as a bare name or string.

- by:

  Parcel-key specification. Use a shared column name such as \`"id"\`,
  or a named character vector to map an atlas metadata column to a
  differently named data column, for example \`c(id = "roi_index")\`.
  Composite keys are supported. When \`NULL\`, a safe unique key is
  inferred.

- allow_partial:

  Logical. If \`FALSE\` (default), \`data\` must contain every atlas
  parcel. If \`TRUE\`, unmatched atlas parcels receive \`NA\`. Unknown
  and duplicate data keys always error.

## Value

A numeric vector named by parcel ID and ordered exactly like
\`atlas\$ids\`.

## Details

With \`by = NULL\`, \`id\` is preferred, followed by \`label_full\`.
Composite label keys are considered only when they are unique in both
the atlas and data. Ambiguous short labels therefore fail instead of
being recycled across hemispheres or networks.

Atlas metadata columns supplied in \`data\` are treated as consistency
checks. They are never allowed to overwrite canonical metadata.

## Examples

``` r
atlas <- structure(
  list(
    name = "toy",
    atlas = array(1:3, c(3, 1, 1)),
    ids = 1:3,
    labels = c("A", "B", "C"),
    orig_labels = c("lh_A", "lh_B", "rh_C"),
    hemi = c("left", "left", "right")
  ),
  class = "atlas"
)
results <- data.frame(
  roi_index = c(3L, 1L, 2L),
  estimate = c(0.3, 0.1, 0.2)
)
align_parcel_values(
  atlas,
  results,
  value = estimate,
  by = c(id = "roi_index")
)
#>   1   2   3 
#> 0.1 0.2 0.3 
```

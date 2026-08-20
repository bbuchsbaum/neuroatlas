# Convert an Object to \`parcel_data\`

Convert an Object to \`parcel_data\`

## Usage

``` r
as_parcel_data(x, ...)

# S3 method for class 'parcel_data'
as_parcel_data(x, ...)

# S3 method for class 'atlas'
as_parcel_data(
  x,
  values = NULL,
  value_col = "value",
  by = NULL,
  allow_partial = FALSE,
  atlas_id = NULL,
  atlas_version = NULL,
  atlas_space = NULL,
  schema_version = "1.0.0",
  ...
)

# Default S3 method
as_parcel_data(x, ...)
```

## Arguments

- x:

  Object to convert.

- ...:

  Additional arguments passed to methods.

- values:

  Optional values to attach to parcel rows. - numeric/integer vector of
  length \`length(x\$ids)\` - data frame/tibble with a stable parcel key
  and one or more value columns. Use \`by\` for renamed or composite
  keys.

- value_col:

  Column name used when \`values\` is a vector.

- by:

  Parcel-key specification used when \`values\` is a data frame. Unnamed
  values join columns with the same name, for example \`"id"\` or
  \`c("label", "hemi", "network")\`. A named character vector maps atlas
  metadata columns to columns in \`values\`, following dplyr join
  syntax, for example \`c(id = "roi_index")\`. When \`NULL\`, a safe
  unique key is inferred.

- allow_partial:

  Logical. If \`FALSE\` (default), a data-frame input must contain
  exactly one row for every atlas parcel. If \`TRUE\`, missing parcels
  are retained with \`NA\` values. Rows whose keys do not occur in the
  atlas always produce an error.

- atlas_id:

  Optional canonical atlas id override.

- atlas_version:

  Optional atlas version.

- atlas_space:

  Optional atlas space/template identifier.

- schema_version:

  Schema version for the returned object.

## Value

An object of class \`"parcel_data"\`.

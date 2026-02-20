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
  length \`length(x\$ids)\` - data frame/tibble with join column \`id\`
  or \`label\` and one or more value columns

- value_col:

  Column name used when \`values\` is a vector.

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

# Parcel-Level Data Container

Create a validated, serializable parcel-level data object.

## Usage

``` r
parcel_data(
  parcels,
  atlas_id,
  atlas_name = atlas_id,
  atlas_version = NULL,
  atlas_space = NULL,
  schema_version = "1.0.0"
)
```

## Arguments

- parcels:

  A data frame or tibble with one row per parcel.

- atlas_id:

  Canonical parcellation identifier.

- atlas_name:

  Human-readable parcellation name. Defaults to \`atlas_id\`.

- atlas_version:

  Optional atlas version string.

- atlas_space:

  Optional template/space identifier.

- schema_version:

  Schema version string. Default: \`"1.0.0"\`.

## Value

An object of class \`"parcel_data"\`.

## Details

\`parcel_data\` formalizes reduced parcel representations as two
components: - atlas identity metadata (\`atlas\`) - a parcel table
(\`parcels\`) with required columns \`id\`, \`label\`, and \`hemi\`

Additional columns in \`parcels\` are interpreted as value or feature
columns.

## See also

\[as_parcel_data()\], \[write_parcel_data()\], \[read_parcel_data()\]

## Examples

``` r
tbl <- tibble::tibble(
  id = c(1L, 2L),
  label = c("A", "B"),
  hemi = c("left", "right"),
  statistic = c(0.4, -0.2)
)
x <- parcel_data(tbl, atlas_id = "toy_atlas")
x
#> parcel_data
#>   schema: 1.0.0 
#>   atlas: toy_atlas 
#>   parcels: 2 
#>   value_cols: statistic 
```

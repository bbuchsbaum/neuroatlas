# Write a \`parcel_data\` Object to Disk

Write a \`parcel_data\` Object to Disk

## Usage

``` r
write_parcel_data(x, file, format = c("auto", "rds", "json"), pretty = TRUE)
```

## Arguments

- x:

  A \`parcel_data\` object.

- file:

  Output file path.

- format:

  Serialization format: \`"auto"\`, \`"rds"\`, or \`"json"\`.

- pretty:

  Logical; pretty-print JSON output when \`format = "json"\`.

## Value

Invisibly returns normalized output path.

## Examples

``` r
x <- parcel_data(data.frame(
  id = 1:2, label = c("A", "B"),
  hemi = c("left", "right"), value = c(0.2, 0.7)
), atlas_id = "toy")
file <- tempfile(fileext = ".rds")
write_parcel_data(x, file)
read_parcel_data(file)
#> parcel_data
#>   schema: 1.0.0 
#>   atlas: toy 
#>   parcels: 2 
#>   value_cols: value 
unlink(file)
```

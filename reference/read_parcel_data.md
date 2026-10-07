# Read a \`parcel_data\` Object from Disk

Read a \`parcel_data\` Object from Disk

## Usage

``` r
read_parcel_data(file, format = c("auto", "rds", "json"), validate = TRUE)
```

## Arguments

- file:

  Input file path.

- format:

  Serialization format: \`"auto"\`, \`"rds"\`, or \`"json"\`.

- validate:

  Logical. If \`TRUE\` (default), validate after reading.

## Value

A \`parcel_data\` object.

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

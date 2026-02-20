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

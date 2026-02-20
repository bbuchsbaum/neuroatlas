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

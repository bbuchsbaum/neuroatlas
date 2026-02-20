# Validate a Parcel-Level Data Object

Validate structure and key invariants for \`parcel_data\` objects.

## Usage

``` r
validate_parcel_data(x, strict = TRUE)
```

## Arguments

- x:

  An object expected to be \`parcel_data\`.

- strict:

  Logical. If \`TRUE\` (default), enforce strict checks on atlas
  metadata consistency.

## Value

Invisibly returns \`x\` if valid; otherwise throws an error.

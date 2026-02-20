# Extract Parcel Values Aligned to an Atlas

Returns a vector aligned to \`atlas\$ids\`, suitable for \`map_atlas()\`
or \`plot_brain()\`.

## Usage

``` r
parcel_values(x, atlas, column = "value")
```

## Arguments

- x:

  A \`parcel_data\` object.

- atlas:

  An atlas object.

- column:

  Value column in \`x\$parcels\` to extract.

## Value

A vector with \`length(atlas\$ids)\` elements ordered to \`atlas\$ids\`.

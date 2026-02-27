# Convert Atlas Connectivity to igraph

Convert Atlas Connectivity to igraph

## Usage

``` r
as_igraph(x, ...)

# S3 method for class 'atlas_connectivity'
as_igraph(x, weighted = TRUE, ...)
```

## Arguments

- x:

  An `atlas_connectivity` matrix.

- ...:

  Additional arguments (currently ignored).

- weighted:

  Logical. If `TRUE` (default), edge weights are the correlation values.
  If `FALSE`, a binary adjacency is used.

## Value

Dispatches to methods.

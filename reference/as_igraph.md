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

## Examples

``` r
if (requireNamespace("igraph", quietly = TRUE)) {
  connectivity <- structure(matrix(c(0, 0.5, 0.5, 0), 2),
    class = c("atlas_connectivity", "matrix", "array")
  )
  as_igraph(connectivity)
}
#> IGRAPH b6106e9 U-W- 2 1 -- 
#> + attr: weight (e/n)
#> + edge from b6106e9:
#> [1] 1--2
```

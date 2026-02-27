# Compute Parcel Adjacency Graph from an Atlas

Builds a region adjacency graph from a volumetric atlas. Two parcels are
considered adjacent when at least one pair of their voxels are
neighbours under the chosen connectivity scheme.

## Usage

``` r
atlas_graph(
  atlas,
  connectivity = c("6", "18", "26"),
  as = c("matrix", "igraph", "tibble"),
  include_weight = TRUE
)
```

## Arguments

- atlas:

  An atlas object (must contain `$atlas`, `$ids`, and `$labels`).

- connectivity:

  Character. Neighbourhood type: `"6"` (face), `"18"` (face + edge), or
  `"26"` (face + edge + corner). Default: `"6"`.

- as:

  Character. Output format: `"matrix"` (K x K adjacency matrix),
  `"igraph"` (igraph graph object), or `"tibble"` (edge-list data frame
  with `from`, `to`, `weight` columns). Default: `"matrix"`.

- include_weight:

  Logical. If `TRUE` (default), edge values are the count of shared
  boundary voxel pairs. If `FALSE`, edges are binary (0/1).

## Value

Depending on `as`:

- matrix:

  A symmetric integer matrix of dimension K x K with row/column names
  set to region labels.

- igraph:

  An undirected igraph graph (requires the igraph package).

- tibble:

  A `data.frame` (tibble if available) with columns `from`, `to`, and
  `weight`.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(100)
adj   <- atlas_graph(atlas)
adj_b <- atlas_graph(atlas, include_weight = FALSE)
el    <- atlas_graph(atlas, as = "tibble")
} # }
```

# Cross-Atlas Overlap Analysis

Computes spatial overlap between regions of two atlases using Dice
and/or Jaccard similarity coefficients. This is useful for comparing
parcellations, assessing correspondence between atlases, or mapping
regions from one atlas to another.

## Usage

``` r
atlas_overlap(atlas1, atlas2, metrics = c("dice", "jaccard"), min_overlap = 0)
```

## Arguments

- atlas1:

  An atlas object (with `$atlas`, `$ids`, `$labels`).

- atlas2:

  An atlas object (with `$atlas`, `$ids`, `$labels`).

- metrics:

  Character vector of overlap metrics to compute. One or more of
  `"dice"` and `"jaccard"`. Default is both.

- min_overlap:

  Integer. Minimum number of overlapping voxels for a pair to be
  included in the results. Default is 0 (include all pairs with any
  overlap).

## Value

A `tibble` with columns:

- atlas1_id:

  Integer region ID from `atlas1`

- atlas1_label:

  Character label from `atlas1`

- atlas2_id:

  Integer region ID from `atlas2`

- atlas2_label:

  Character label from `atlas2`

- dice:

  Dice similarity coefficient (if requested)

- jaccard:

  Jaccard similarity coefficient (if requested)

- n_overlap:

  Number of overlapping voxels

- n_atlas1:

  Total voxels in the `atlas1` region

- n_atlas2:

  Total voxels in the `atlas2` region

Rows are sorted by Dice coefficient descending (or Jaccard if Dice was
not requested).

## Examples

``` r
if (FALSE) { # \dontrun{
atlas_a <- get_schaefer_atlas(parcels = "100", networks = "7")
atlas_b <- get_schaefer_atlas(parcels = "200", networks = "7")
overlap <- atlas_overlap(atlas_a, atlas_b)
head(overlap)
} # }
```

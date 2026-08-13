# Dilating Atlas Parcels Safely

A parcellation can leave small gaps between labelled regions. If those
gaps are valid analysis voxels, regional summaries silently omit them.
Dilation can close the gaps, but only after you state two contracts:
which voxels are eligible, and how far a parcel may grow.

This article uses a tiny synthetic atlas so every assignment can be
inspected and every claim is executed offline. The same rules apply to a
whole-brain atlas.

## What does dilation do?

[`dilate_atlas()`](../reference/dilate_atlas.md) considers voxels that
are inside a supplied mask but not already labelled. A candidate is
assigned only when a labelled voxel lies within `radius`, measured in
**voxel units**. When several parcels are nearby,
inverse-distance-weighted voting over at most `maxn` neighbours chooses
the label.

The function never decides what anatomy is appropriate. That is the
mask’s job.

## How do you build a transparent example?

Our atlas has two one-voxel parcels on a straight line. IDs are
deliberately non-contiguous so a label-mapping error cannot pass
unnoticed.

``` r

library(neuroatlas)

dims <- c(9L, 9L, 5L)
space <- neuroim2::NeuroSpace(
  dim = dims,
  spacing = c(2, 2, 2),
  origin = c(0, 0, 0)
)

labels <- array(0L, dim = dims)
labels[2, 5, 3] <- 1L
labels[8, 5, 3] <- 5L

toy_atlas <- structure(
  list(
    name = "two-parcel example",
    atlas = neuroim2::NeuroVol(labels, space),
    ids = c(1L, 5L),
    labels = c("A", "B"),
    orig_labels = c("A", "B"),
    hemi = c(NA_character_, NA_character_)
  ),
  class = c("toy_atlas", "atlas")
)
```

The eligible mask is a one-voxel-wide bridge between A and B. One
additional voxel sits far away and demonstrates the radius guard.

``` r

eligible <- array(FALSE, dim = dims)
eligible[2:8, 5, 3] <- TRUE
eligible[9, 9, 5] <- TRUE

mask <- neuroim2::LogicalNeuroVol(eligible, space)
```

## What changes with a small radius?

A radius of one voxel grows each parcel by one grid step. The middle of
the bridge and the distant candidate remain unassigned.

``` r

radius_one <- dilate_atlas(
  toy_atlas,
  mask,
  radius = 1,
  maxn = 2
)

before <- neuroim2::as.dense(toy_atlas$atlas)
after_one <- neuroim2::as.dense(radius_one$atlas)

c(
  labelled_before = sum(before > 0),
  labelled_after = sum(after_one > 0),
  eligible = sum(eligible)
)
#> labelled_before  labelled_after        eligible 
#>               2               4               8
```

## What changes with a larger radius?

At three voxels, every point along the bridge can reach a parcel. The
distant voxel is still protected because it lies more than three grid
steps from the nearest label.

``` r

radius_three <- dilate_atlas(
  toy_atlas,
  mask,
  radius = 3,
  maxn = 2
)

after_three <- neuroim2::as.dense(radius_three$atlas)

c(
  labelled_before = sum(before > 0),
  labelled_after = sum(after_three > 0),
  eligible = sum(eligible)
)
#> labelled_before  labelled_after        eligible 
#>               2               7               8
```

The radius changed proximity, not anatomy. If the far voxel had been
close enough, the algorithm would have labelled it because the mask
declared it eligible.

## How do you see the assignment?

The table below shows the bridge from left to right. Zero means
unassigned.

``` r

data.frame(
  x = 2:8,
  before = before[2:8, 5, 3],
  radius_1 = after_one[2:8, 5, 3],
  radius_3 = after_three[2:8, 5, 3]
)
#>   x before radius_1 radius_3
#> 1 2      1        1        1
#> 2 3      0        1        1
#> 3 4      0        0        1
#> 4 5      0        0        1
#> 5 6      0        0        5
#> 6 7      0        5        5
#> 7 8      5        5        5
```

The midpoint is equidistant. Its label is a deterministic consequence of
the grouped label ordering used to break an equal vote, so do not
interpret a tie as anatomical evidence. In real work, inspect parcel
boundaries and quantify sensitivity to radius.

## What must be true for real data?

Before dilating a real atlas, verify all of the following:

1.  **Same template space.** The atlas and mask describe the same
    anatomy.
2.  **Same voxel grid.** Dimensions and spatial geometry match exactly.
3.  **Appropriate mask.** A cortical atlas should use a cortical ribbon
    or an equivalently justified mask, not an unrestricted
    whole-grey-matter mask.
4.  **Interpretable radius.** Convert voxel units to millimetres using
    the grid spacing, and report both.
5.  **Stable identities.** The output contains only the input parcel
    IDs.
6.  **Sensitivity.** Report how many voxels are added and where they are
    added for plausible radii.

Grid resampling does not transform one anatomical template into another.
For example, the nonlinear route between `MNI152NLin6Asym` and
`MNI152NLin2009cAsym` is currently recorded as planned:

``` r

atlas_transform_plan(
  "MNI152NLin6Asym",
  "MNI152NLin2009cAsym",
  data_type = "voxel"
)
#> <atlas_transform_plan>
#>   from_space: MNI152NLin6Asym 
#>   to_space: MNI152NLin2009cAsym 
#>   n_steps: 1 
#>   status: planned 
#>   confidence: high 
#>   warnings: Plan includes unimplemented/planned transform step(s).
```

Do not build a mask in one of those spaces, reslice it onto the other’s
grid, and treat the result as registered anatomy unless an actual
validated transform has been applied.

## How do you use the result?

The returned object preserves the atlas class, IDs, and labels, so it
can enter the same volume workflows as its input:

``` r

image <- neuroim2::NeuroVol(
  array(seq_len(prod(dims)), dim = dims),
  space
)

reduce_atlas(radius_three, image, mean)
#> # A tibble: 2 × 2
#>   region value
#>   <chr>  <dbl>
#> 1 A       202.
#> 2 B       205
```

Return to
[`vignette("neuroatlas-overview")`](../articles/neuroatlas-overview.md)
for the general reduction workflow, or read
[`vignette("working-with-templateflow")`](../articles/working-with-templateflow.md)
before combining external templates and masks.

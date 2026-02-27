# Batch Reduce Multiple Volumes by an Atlas

Applies [`reduce_atlas`](reduce_atlas.md) to multiple input volumes (or
file paths) and combines the results into a single tibble with a
`subject` column identifying each input.

## Usage

``` r
batch_reduce(
  inputs,
  atlas,
  stat_func = mean,
  ...,
  format = "long",
  parallel = FALSE,
  .progress = TRUE
)
```

## Arguments

- inputs:

  A named list of inputs. Each element can be:

  - A `NeuroVol` (3D) or `NeuroVec` (4D) object

  - A character string file path (read via
    [`neuroim2::read_vol`](https://bbuchsbaum.github.io/neuroim2/reference/read_vol.html))

  If unnamed, subjects are auto-named `"sub_001"`, `"sub_002"`, etc.

- atlas:

  An atlas object.

- stat_func:

  Function to apply within each ROI (default: `mean`).

- ...:

  Additional arguments passed to [`reduce_atlas`](reduce_atlas.md).

- format:

  Character, output format passed to `reduce_atlas`. Default `"long"`.

- parallel:

  Logical. If `TRUE`, uses
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
  for parallel processing. Requires the future.apply package.

- .progress:

  Logical. If `TRUE` (default), displays a `cli` progress bar.

## Value

A tibble with a `subject` column prepended to the `reduce_atlas` output
for each input.

## See also

[`reduce_atlas`](reduce_atlas.md) for single-volume extraction

## Examples

``` r
if (FALSE) { # \dontrun{
# With NeuroVol objects
vols <- list(sub01 = vol1, sub02 = vol2, sub03 = vol3)
results <- batch_reduce(vols, atlas, mean)

# With file paths
files <- list(sub01 = "path/to/sub01.nii.gz",
              sub02 = "path/to/sub02.nii.gz")
results <- batch_reduce(files, atlas, mean, parallel = TRUE)
} # }
```

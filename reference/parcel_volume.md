# Expand Parcel Values to a Volumetric Atlas

Align one parcel-level metric to a volumetric atlas and expand it into a
dense
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
in the atlas's native grid and spatial reference.

## Usage

``` r
parcel_volume(
  atlas,
  data,
  value,
  by = NULL,
  allow_partial = FALSE,
  background = NA_real_
)
```

## Arguments

- atlas:

  A volumetric atlas object. Surface atlases are not supported.

- data:

  A data frame, tibble, or `parcel_data` object containing parcel keys
  and values.

- value:

  A numeric value column, supplied as a bare name or string.

- by:

  Parcel-key specification passed to
  [`align_parcel_values()`](align_parcel_values.md).

- allow_partial:

  Logical. If `FALSE` (default), `data` must contain every atlas parcel.
  If `TRUE`, missing parcels receive `NA`. Unknown and duplicate parcel
  keys always error.

- background:

  Numeric scalar used outside the atlas. The default is `NA_real_`,
  which keeps background distinct from valid parcel values such as zero.

## Value

A dense
[`neuroim2::NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
containing the selected parcel value at every voxel belonging to that
parcel. Atlas space and geometry are preserved.

## Details

`parcel_volume()` uses the same strict key matching as
[`align_parcel_values()`](align_parcel_values.md). The input table
remains the authoritative result; the returned volume is a deterministic
rendering representation. Atlas voxels whose non-zero labels are absent
from `atlas$ids` cause an error rather than being silently treated as
background.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_aseg_atlas()
results <- data.frame(
  id = atlas$ids,
  z_stat = stats::rnorm(length(atlas$ids))
)
z_map <- parcel_volume(atlas, results, z_stat)
} # }
```

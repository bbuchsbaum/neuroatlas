# Load the Extended HCP Atlas (HCPex)

Load HCPex v1.1, a volumetric atlas with 360 cortical and 66 subcortical
regions in the source-declared MNI152NLin2009cAsym template space.

## Usage

``` r
get_hcpex_atlas(resolution = c("1", "2"), outspace = NULL, use_cache = TRUE)
```

## Source

<https://github.com/wayalan/HCPex>, including the v1.1 user guide.

## Arguments

- resolution:

  Native voxel size in millimetres: `1` (default) or `2`.

- outspace:

  Optional three-dimensional `NeuroSpace`. Resampling uses nearest
  neighbours and changes the sampling grid only; it does not register
  the atlas to a different anatomical template. Small regions may
  disappear.

- use_cache:

  Logical. Reuse checksum-verified files in the neuroatlas user cache.
  If `FALSE`, download temporary files and remove them after loading.

## Value

An object with classes `hcpex` and `atlas`, containing a `NeuroVol`,
region IDs, labels, hemisphere assignments, RGB colors, and resource
metadata.

## Details

Files are downloaded on demand from a pinned revision of the upstream
v1.1 distribution. Both native resolutions retain the upstream voxel
geometry and HCPex IDs (1:426, with zero as background). HCPex cortical
ordering differs from the original HCP-MMP1.0 ordering; matrices in that
ordering must be reordered before use with this atlas.

Labels combine upstream abbreviations with hemisphere suffixes (e.g.,
`V1_L`, `V1_R`). `orig_labels` preserves the full lookup-table names.
[`roi_metadata()`](roi_metadata.md) also includes `region` (the
unsuffixed abbreviation) and `division` (`"cortical"` for IDs 1:360,
`"subcortical"` for 361:426). The complete region catalogue is retained
after resampling, even when some regions no longer have voxels. No
surface representation is supplied.

The upstream data are distributed under GPL-3.0; their license, source
revision, file receipts, and publication references are attached to the
atlas.

## References

Huang CC, Rolls ET, Feng J, Lin CP (2022). An extended Human Connectome
Project multimodal parcellation atlas of the human cortex and
subcortical areas. Brain Structure and Function, 227, 763-778.
[doi:10.1007/s00429-021-02421-6](https://doi.org/10.1007/s00429-021-02421-6)

Huang CC, Rolls ET, Hsu CH, Feng J, Lin CP (2021). Extensive Cortical
Connectivity of the Human Hippocampal Memory System: Beyond the "What"
and "Where" Dual Stream Model. Cerebral Cortex.
[doi:10.1093/cercor/bhab113](https://doi.org/10.1093/cercor/bhab113)

## See also

[`get_atlas()`](get_atlas.md),
[`get_glasser_atlas()`](get_glasser_atlas.md),
[`atlas_citations()`](atlas_citations.md)

## Examples

``` r
if (FALSE) { # \dontrun{
hcp <- get_hcpex_atlas(resolution = 2)
# Also available through get_atlas("hcpex", resolution = 2)
get_roi(hcp, label = "V1_L")
subcortex <- filter_atlas(hcp, division == "subcortical")
atlas_metadata(hcp)
atlas_citations(hcp)
} # }
```

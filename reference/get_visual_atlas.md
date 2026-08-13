# Early Visual Cortex Atlas (V1-V5, cytoarchitectonic)

A convenience loader that extracts the early visual areas from the
Julich-Brain cytoarchitectonic atlas and relabels them as `V1`-`V5` per
hemisphere. This provides a compact, probabilistically-derived
early-visual parcellation in MNI volume space without the surrounding
whole-brain regions.

## Usage

``` r
get_visual_atlas(
  outspace = NULL,
  smooth = FALSE,
  resolution = NULL,
  fsl_dir = Sys.getenv("FSLDIR"),
  download = TRUE
)
```

## Arguments

- outspace:

  Optional `NeuroSpace` object to resample the atlas into.

- smooth:

  Logical. Whether to smooth parcel boundaries when resampling.

- resolution:

  Optional Julich-Brain resolution (e.g. `"1mm"` or `"2mm"`); passed to
  [`get_julich_brain_atlas()`](get_julich_brain_atlas.md).

- fsl_dir:

  FSL installation directory. Defaults to `Sys.getenv("FSLDIR")`; when
  empty and `download = TRUE` the Julich-Brain FSL cache is downloaded.

- download:

  Logical. Download the Julich-Brain FSL cache when `fsl_dir` is unset.

## Value

A list with classes `c("visual", "volatlas", "atlas")` containing the
V1-V5 regions per hemisphere.

## Details

The source regions are the Julich-Brain maximum-probability labels
`GM Visual cortex V1 BA17`, `V2 BA18`, `V3V`, `V4`, and `V5`
(left/right), loaded via
[`get_julich_brain_atlas()`](get_julich_brain_atlas.md). Note that the
Julich atlas defines only the ventral subdivision of V3 (`V3V`) and a
single `V4`/`V5` region per hemisphere.

For the topographic surface atlas with dorsal/ventral subdivisions and
`hV4`, see [`get_wang_atlas`](get_wang_atlas.md); for a volumetric
functional atlas, see [`get_visfatlas`](get_visfatlas.md).

## References

Amunts, K., Mohlberg, H., Bludau, S., & Zilles, K. (2020). Julich-Brain:
A 3D probabilistic atlas of the human brain's cytoarchitecture. Science,
369(6506), 988-992.
[doi:10.1126/science.abb4588](https://doi.org/10.1126/science.abb4588)

## See also

[`get_wang_atlas`](get_wang_atlas.md),
[`get_visfatlas`](get_visfatlas.md),
[`get_julich_brain_atlas`](get_julich_brain_atlas.md).

## Examples

``` r
if (FALSE) { # \dontrun{
v <- get_visual_atlas()
v$labels
get_roi(v, label = "V1", hemi = "left")
} # }
```

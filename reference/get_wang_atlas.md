# Wang (2015) Probabilistic Visual Topography Atlas

Load the Wang et al. (2015) probabilistic atlas of visual topographic
areas as a pair of neurosurf `LabeledNeuroSurface` objects on the
FreeSurfer `fsaverage` surface. The atlas defines 25 topographic areas
per hemisphere, covering the early visual areas (`V1v`, `V1d`, `V2v`,
`V2d`, `V3v`, `V3d`, `hV4`) plus ventral, lateral, dorsal, and parietal
maps (`VO1/2`, `PHC1/2`, `LO1/2`, `TO1/2`, `V3A/B`, `IPS0-5`, `SPL1`,
`FEF`).

## Usage

``` r
get_wang_atlas(
  surf = c("midthickness", "pial", "white"),
  space = "fsaverage",
  use_cache = TRUE
)
```

## Source

<https://github.com/noahbenson/neuropythy> (bundled Wang 2015 atlas);
original distribution at <https://scholar.princeton.edu/napl/resources>.

## Arguments

- surf:

  Surface type. One of `"midthickness"` (default), `"pial"`, or
  `"white"`. (TemplateFlow does not provide an inflated fsaverage
  surface.)

- space:

  Surface space / mesh template. Only `"fsaverage"` (164k vertices) is
  supported, matching the native atlas resolution.

- use_cache:

  Logical. Whether to use cached downloads. Default `TRUE`.

## Value

A list with classes `c("wang", "surfatlas", "atlas")` containing
`lh_atlas`/`rh_atlas` (`LabeledNeuroSurface` objects), `ids`, `labels`,
`hemi`, `cmap`, and the standard atlas provenance metadata.

## Details

The surface labels are the maximum-probability map (MPM) derived from 53
subjects, distributed as `fsaverage` (164k vertices) FreeSurfer overlay
files (`lh/rh.wang15_mplbl.v1_0.mgz`) bundled with the neuropythy
library. Files are downloaded on demand from the neuropythy GitHub
repository and cached under the neuroatlas user cache directory.

Surface geometry is obtained from TemplateFlow via
[`get_surface_template`](get_template.md), so a working TemplateFlow
setup is required (mirroring [`glasser_surf`](glasser_surf.md)). The
volumetric counterparts (maximum-probability and per-area probability
maps in MNI space) are served by
[`get_wang_prob_atlas`](get_wang_prob_atlas.md).

TemplateFlow does not currently distribute an *inflated* `fsaverage`
mesh, so the available surfaces are `"midthickness"` (default),
`"pial"`, and `"white"`.

## References

Wang, L., Mruczek, R. E. B., Arcaro, M. J., & Kastner, S. (2015).
Probabilistic Maps of Visual Topography in Human Cortex. Cerebral
Cortex, 25(10), 3911-3931.
[doi:10.1093/cercor/bhu277](https://doi.org/10.1093/cercor/bhu277)

## See also

[`get_wang_prob_atlas`](get_wang_prob_atlas.md) for the full per-area
probability volumes, [`get_visfatlas`](get_visfatlas.md) for a
volumetric visual-cortex atlas,
[`get_visual_atlas`](get_visual_atlas.md) for cytoarchitectonic V1-V5.

## Examples

``` r
if (FALSE) { # \dontrun{
# Wang 2015 visual topography atlas on the fsaverage midthickness surface
wang <- get_wang_atlas(surf = "midthickness")
wang$labels
# Extract an area as ROISurface objects (one per hemisphere)
get_roi(wang, label = "hV4")
get_roi(wang, label = "V1v", hemi = "left")
} # }
```

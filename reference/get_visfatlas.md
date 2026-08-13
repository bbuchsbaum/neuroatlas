# visfAtlas Probabilistic Functional Visual Atlas (volume)

Load the Rosenke et al. (2021) probabilistic functional atlas of human
occipito-temporal visual cortex ("visfAtlas") as a volumetric MNI atlas.
The maximum-probability labelmap contains 33 regions spanning the early
retinotopic areas (`v1d`, `v2d`, `v3d`, `v1v`, `v2v`, `v3v`),
motion-selective `hMT`, and category-selective regions for faces,
bodies, characters, and places.

## Usage

``` r
get_visfatlas(outspace = NULL, smooth = FALSE, use_cache = TRUE)
```

## Source

<https://download.brainvoyager.com/data/visfAtlas.zip>

## Arguments

- outspace:

  Optional `NeuroSpace` object (or TemplateFlow-style descriptor) to
  resample the atlas into.

- smooth:

  Logical. Whether to smooth parcel boundaries when resampling.

- use_cache:

  Logical. Whether to use cached downloads. Default `TRUE`.

## Value

A list with classes `c("visfatlas", "volatlas", "atlas")`.

## Details

The atlas is distributed as a single archive (`visfAtlas.zip`, ~70 MB)
containing FreeSurfer, BrainVoyager, and NIfTI representations. This
loader downloads the archive on demand, extracts the volumetric
maximum-probability map (`visfAtlas_MNI152_volume.nii.gz`, 1 mm), and
caches both under the neuroatlas user cache directory. Region
intensities (1-33) follow the distributed FSL atlas specification;
region names use the lower/upper-case source labels with hemisphere
prefixes `lh_`/`rh_`.

The volume is a single-subject MNI-space grid (182 x 218 x 182, 1 mm);
the publication aligns it to the MNI colin27 brain.

Note that the visfAtlas defines V1-V3 (dorsal and ventral) but not hV4;
for V4 see [`get_wang_atlas`](get_wang_atlas.md) or
[`get_visual_atlas`](get_visual_atlas.md).

## References

Rosenke, M., van Hoof, R., van den Hurk, J., Grill-Spector, K., &
Goebel, R. (2021). A Probabilistic Functional Atlas of Human
Occipito-Temporal Visual Cortex. Cerebral Cortex, 31(1), 603-619.
[doi:10.1093/cercor/bhaa246](https://doi.org/10.1093/cercor/bhaa246)

## See also

[`get_wang_atlas`](get_wang_atlas.md),
[`get_visual_atlas`](get_visual_atlas.md).

## Examples

``` r
if (FALSE) { # \dontrun{
visf <- get_visfatlas()
table(visf$hemi)
get_roi(visf, label = "lh_v1d_retinotopic")
} # }
```

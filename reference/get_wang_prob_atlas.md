# Wang (2015) Full Per-Area Probability Volumes (Princeton ProbAtlas_v4)

Resolve (and optionally load) the full per-area probability maps and
maximum-probability volumes from the original Princeton ProbAtlas_v4
distribution of the Wang et al. (2015) visual topography atlas. These
are the volumetric counterparts to the surface labels returned by
[`get_wang_atlas()`](get_wang_atlas.md): for each of the 25 topographic
areas there is a continuous probability map
(`perc_VTPM_vol_roi<n>_<hemi>.nii.gz`) in MNI volume space, plus a
maximum-probability summary (`maxprob_vol_<hemi>.nii.gz`).

## Usage

``` r
get_wang_prob_atlas(
  prob_dir = NULL,
  image = c("probability", "maxprob"),
  hemi = c("both", "lh", "rh"),
  rois = NULL,
  path_only = TRUE,
  use_cache = TRUE
)

# S3 method for class 'wang_prob_paths'
print(x, ...)
```

## Source

<https://napl.scholar.princeton.edu/resources>

## Arguments

- prob_dir:

  Optional path to a locally-extracted ProbAtlas_v4 directory (the
  folder containing `subj_vol_all`, or that subfolder itself). When
  supplied it is used exclusively; otherwise the cached/downloaded data
  is used.

- image:

  One of `"probability"` (per-area `perc_VTPM` maps, the default) or
  `"maxprob"` (the maximum-probability summary volume).

- hemi:

  One of `"both"` (default), `"lh"`, or `"rh"`.

- rois:

  Optional subset of areas, given as labels (e.g. `c("V1v", "hV4")`) or
  integer ids (1-25). `NULL` selects all 25.

- path_only:

  Logical. When `TRUE` (default), return a manifest of paths/metadata
  without downloading or reading image data. When `FALSE`, read the
  resolved volumes as `NeuroVol` objects, downloading them to the cache
  on first use if needed.

- use_cache:

  Logical. Look for (and download into) the neuroatlas Wang cache
  directory when `prob_dir` is not supplied. `FALSE` forces a fresh
  download on load.

- x:

  A `wang_prob_paths` object.

- ...:

  Unused.

## Value

When `path_only = TRUE`, an object of class `wang_prob_paths`: a list
with the requested `image`/`hemi`, the `resources_url` and
`download_url`, a `files` tibble (`id`, `label`, `hemi`, `member`,
`path`, `exists`), and the canonical `labels`. When `path_only = FALSE`,
a `wang_prob_volumes` list whose `volumes` element holds the loaded
`NeuroVol` objects.

## Details

On first load the volumes (~0.7 MB) are downloaded from the neuroatlas
GitHub release and cached under the neuroatlas Wang cache directory,
after which they resolve offline. They originate from the Princeton
ProbAtlas_v4 distribution (Wang et al. 2015), whose original host
(`napl.scholar.princeton.edu`) blocks scripted downloads and is no
longer reliably available; the archive carried no licence and the maps
are widely redistributed as an open-science resource, so they are
re-hosted on the package's own release for programmatic access.

To use a local copy instead (e.g. an updated release), download/unzip
`ProbAtlas_v4` and pass the resulting directory (or its `subj_vol_all`
subfolder) via `prob_dir`; it takes precedence and is never mixed with
the cache or download. Set `path_only = FALSE` to read the requested
volumes as `NeuroVol` objects.

The resolution order on a load is `prob_dir` -\> cache -\> download. A
read-only manifest (`path_only = TRUE`) resolves from `prob_dir`/cache
only and never downloads or writes to the cache. When
`path_only = FALSE` and `use_cache = TRUE`, volumes read from a
user-supplied `prob_dir` are also copied (best-effort, atomically) into
the cache.

Note on naming: the volume coding (`ROIfiles_Labeling.txt`) labels areas
12/13 as `MST`/`hMT`, which correspond to `TO2`/`TO1` in the surface
(neuropythy) naming used by [`get_wang_atlas()`](get_wang_atlas.md); the
numeric ids are identical.

## References

Wang, L., Mruczek, R. E. B., Arcaro, M. J., & Kastner, S. (2015).
Probabilistic Maps of Visual Topography in Human Cortex. Cerebral
Cortex, 25(10), 3911-3931.
[doi:10.1093/cercor/bhu277](https://doi.org/10.1093/cercor/bhu277)

## See also

[`get_wang_atlas`](get_wang_atlas.md) for the fsaverage surface atlas.

## Examples

``` r
if (FALSE) { # \dontrun{
# Manifest only (no download): what is available and where it comes from
manifest <- get_wang_prob_atlas()
manifest
head(manifest$files)

# Load the maximum-probability volumes (downloaded + cached on first use)
wp <- get_wang_prob_atlas(image = "maxprob", path_only = FALSE)

# Load a single area's probability map
v1v_lh <- get_wang_prob_atlas(rois = "V1v", hemi = "lh", path_only = FALSE)
} # }
```

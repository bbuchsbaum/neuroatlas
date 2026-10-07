# Project a Volume onto a Surface Atlas

Samples a volumetric map (for example a thresholded statistic or cluster
map) onto the vertices of both hemispheres of a surface atlas, using the
same volume-to-surface projection that
[`plot_brain`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
applies when its `overlay` argument is a `NeuroVol`. The result holds
one value per atlas vertex, so it can be passed back to
`plot_brain(overlay = )`, summarised, or used to build a colour key that
matches the rendered surface exactly.

## Usage

``` r
project_cluster_overlay(
  cluster_vol,
  surfatlas,
  space_override = NULL,
  density_override = NULL,
  resolution_override = NULL,
  fun = c("avg", "nn", "mode"),
  sampling = c("midpoint", "normal_line", "thickness"),
  interpolation = c("legacy", "nearest", "linear"),
  aggregate = NULL,
  n_samples = NULL,
  depth = NULL,
  surface_smooth_fwhm = 0
)
```

## Arguments

- cluster_vol:

  A
  [`NeuroVol`](https://bbuchsbaum.github.io/neuroim2/reference/NeuroVol.html)
  in the world (mm) space of the surface meshes, typically MNI152 for
  fsaverage surfaces. Zero voxels are treated as data, so mask the
  volume first if zero should mean "no signal".

- surfatlas:

  A surface atlas (class `"surfatlas"`), for example from
  [`schaefer_surf`](https://bbuchsbaum.github.io/neuroatlas/reference/schaefer_surf.md).
  Its vertex count per hemisphere sets the length of the returned
  vectors.

- space_override, density_override, resolution_override:

  Optional surface space (e.g. `"fsaverage6"`), TemplateFlow density,
  and resolution used to look up the white and pial meshes. By default
  these come from the atlas (`surfatlas$surface_space`, falling back to
  `"fsaverage6"`).

- fun:

  Vertex summary passed to
  [`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html):
  one of `"avg"`, `"nn"`, or `"mode"`.

- sampling:

  Sampling strategy between the white and pial surfaces: `"midpoint"`,
  `"normal_line"`, or `"thickness"`.

- interpolation:

  Voxel interpolation: `"legacy"`, `"nearest"`, or `"linear"`.

- aggregate:

  Optional aggregation across depth samples (`"mean"`, `"mode"`, or
  `"closest"`).

- n_samples:

  Optional number of sampling depths.

- depth:

  Optional explicit thickness fractions or normal-line offsets.

- surface_smooth_fwhm:

  Tangential surface smoothing in mm (default `0`, no smoothing).

## Value

A list with two elements:

- overlay:

  A list with numeric vectors `lh` and `rh`, one value per vertex of the
  corresponding atlas hemisphere. Vertices the projection does not reach
  are `NA`. A hemisphere missing from the atlas is `NULL`.

- meta:

  Projection provenance: `surface_space` and, per hemisphere, the vertex
  counts and the sampling settings that were applied (the same record
  [`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
  stores).

## Details

White and pial meshes are taken from the atlas when it was built on them
and otherwise loaded for the atlas's surface space, so the projection is
anatomically correct even for atlases displayed on inflated surfaces.

If projection fails for a hemisphere (for example because the meshes
cannot be loaded or do not match the atlas), that hemisphere is returned
as all `NA` and a warning reports the reason;
[`plot_brain()`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md)
keeps its existing silent behaviour.

## See also

[`plot_brain`](https://bbuchsbaum.github.io/neuroatlas/reference/plot_brain.md),
[`neurosurf::vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.html)

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- schaefer_surf(200, 7, space = "fsaverage6", surf = "inflated")
stat <- neuroim2::read_vol("zstat1.nii.gz")
proj <- project_cluster_overlay(stat, atlas,
  sampling = "thickness",
  interpolation = "linear"
)
range(proj$overlay$lh, na.rm = TRUE)

# Draw exactly the projected values
plot_brain(atlas, overlay = proj$overlay, overlay_threshold = 3.1)
} # }
```

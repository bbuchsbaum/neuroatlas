# Extract regions of interest from a surface atlas

[`get_roi()`](get_roi.md) method for surface atlases (class `surfatlas`,
e.g. from [`get_wang_atlas`](get_wang_atlas.md),
[`glasser_surf`](glasser_surf.md), or `schaefer_surf`). It returns one
[`ROISurface`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface.html)
per matched region, carrying the mesh vertices of that area on the
appropriate hemisphere.

Because a surface atlas stores each hemisphere on its own mesh, a
`label` present in both hemispheres yields one ROI per hemisphere, named
`"<label>_left"` / `"<label>_right"`; pass `hemi` to restrict. When
selecting by `id`, results are named by id (ids are unique across
hemispheres: 1..K left, K+1..2K right).

## Usage

``` r
# S3 method for class 'surfatlas'
get_roi(x, label = NULL, id = NULL, hemi = NULL)
```

## Arguments

- x:

  A surface atlas (class `surfatlas`).

- label, id:

  Character labels or integer ids identifying regions; supply exactly
  one.

- hemi:

  Optional `"left"` / `"right"` filter.

## Value

A named list of
[`ROISurface`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface.html)
objects.

## See also

[`get_roi`](get_roi.md)

# Resolve an anatomical underlay for a surface atlas

Uses an explicit metric, then the atlas anatomy metric, otherwise
curvature computed on matching white geometry with five adjacency
averaging steps. Supplied metrics are used unchanged. The result can be
shared by static and interactive renderers. No curvature is inferred
from an inflated surface.

## Usage

``` r
surface_anatomy(surfatlas, hemi = "lh", metric = NULL, source = NULL)
```

## Arguments

- surfatlas:

  A surface atlas.

- hemi:

  Hemisphere, lh or rh (left and right are also accepted).

- metric:

  Optional finite per-vertex metric overriding the atlas.

- source:

  Optional provenance label for the supplied metric.

## Value

A list with metric and provenance. Unavailable computed anatomy is
neutral, with source recorded as neutral_fallback.

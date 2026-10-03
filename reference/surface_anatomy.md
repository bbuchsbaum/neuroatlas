# Resolve an anatomical underlay for a surface atlas

Uses an explicit metric, then the atlas anatomy metric, otherwise a
metric computed from matching white geometry. Supplied metrics are used
unchanged. The result can be shared by static and interactive renderers.
No curvature is inferred from an inflated surface.

## Usage

``` r
surface_anatomy(
  surfatlas,
  hemi = "lh",
  metric = NULL,
  source = NULL,
  type = c("curvature", "sulcal_depth")
)
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

- type:

  Computed metric used when neither \`metric\` nor the atlas supplies
  one: \`"curvature"\` (default) or \`"sulcal_depth"\`.

## Value

A list with metric and provenance. Unavailable computed anatomy is
neutral, with source recorded as neutral_fallback.

## Details

Two computed metrics are available. \`"curvature"\` is mean curvature of
the white surface with five adjacency averaging steps; it resolves
individual folds but looks mottled on inflated displays.
\`"sulcal_depth"\` is a FreeSurfer-style sulcal-depth proxy, the signed
displacement between the white and the displayed (inflated) surface from
\[neurosurf::surface_sulcal_proxy()\]; it gives the broad two-tone
gyral/sulcal pattern used by Workbench and pycortex. When the proxy
cannot be computed (for example on a white display surface) curvature is
used instead and the provenance records that.

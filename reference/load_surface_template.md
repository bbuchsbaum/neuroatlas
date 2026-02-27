# Load a surface template as a neurosurf geometry

Convenience wrapper around [`get_surface_template`](get_template.md)
that downloads (via TemplateFlow) the requested surface geometry and
returns it as a
[`neurosurf::SurfaceGeometry`](https://rdrr.io/pkg/neurosurf/man/SurfaceGeometry.html)
object (or a left/right list).

## Usage

``` r
load_surface_template(
  template_id,
  surface_type,
  hemi = c("L", "R", "both"),
  density = NULL,
  resolution = NULL,
  ...
)
```

## Arguments

- template_id:

  Surface template identifier passed to TemplateFlow (e.g., "fsaverage",
  "fsaverage6", "fsLR").

- surface_type:

  Surface type (e.g., "white", "pial", "inflated", "midthickness").

- hemi:

  Hemisphere to load. One of "L", "R", or "both". If "both", a named
  list with elements `L` and `R` is returned.

- density:

  Optional surface density (TemplateFlow `density` argument).

- resolution:

  Optional resolution string (TemplateFlow `res` argument), e.g., "06"
  for fsaverage6.

- ...:

  Additional arguments forwarded to
  [`get_surface_template`](get_template.md).

## Value

A
[`neurosurf::SurfaceGeometry`](https://rdrr.io/pkg/neurosurf/man/SurfaceGeometry.html)
object when `hemi` is "L" or "R"; a named list of two `SurfaceGeometry`
objects when `hemi` is "both".

## Examples

``` r
if (FALSE) { # \dontrun{
  # fsaverage6 pial surface as NeuroSurface
  lh <- load_surface_template("fsaverage", "pial", hemi = "L",
                              density = "41k", resolution = "06")

  # Both hemispheres of fsLR 32k inflated surface
  both <- load_surface_template("fsLR", "inflated", hemi = "both",
                                density = "32k")
} # }
```

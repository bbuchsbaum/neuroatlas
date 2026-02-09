# Transform Surface Vertices to Volume Space

Convenience function to transform surface vertex coordinates to match a
target volumetric coordinate space.

## Usage

``` r
transform_vertices_to_volume(
  vertices,
  surface_template,
  target_space = "MNI152"
)
```

## Arguments

- vertices:

  Numeric matrix of vertex coordinates (N x 3).

- surface_template:

  Character string identifying the surface template the vertices come
  from (e.g., "fsaverage", "fsLR").

- target_space:

  Character string identifying the target coordinate space (e.g.,
  "MNI152"). Default is "MNI152".

## Value

Transformed vertex coordinates (N x 3 matrix). Returns the input
unchanged if no transform is needed.

## Details

This is a convenience wrapper around
[`transform_coords`](transform_coords.md) that automatically determines
the source space from the surface template.

## See also

[`transform_coords`](transform_coords.md),
[`get_surface_coordinate_space`](get_surface_coordinate_space.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load fsaverage surface
surf <- load_surface_template("fsaverage", "white", hemi = "L")
verts <- neurosurf::vertices(surf)

# Transform vertices to MNI152 for sampling from fMRI volume
verts_mni152 <- transform_vertices_to_volume(verts, "fsaverage", "MNI152")
} # }
```

# Look Up Atlas Regions at MNI Coordinates

Given one or more 3D coordinates, look up the atlas region(s) at each
location. Supports exact voxel lookup (\`radius = 0\`) or fuzzy search
(\`radius \> 0\`) that returns all regions within a sphere of given
radius in millimetres. Multiple atlases can be queried simultaneously.

## Usage

``` r
query_point(coords, atlas, radius = 0, from_space = "MNI152")
```

## Arguments

- coords:

  Numeric vector of length 3 (single point) or an N x 3 matrix of world
  coordinates.

- atlas:

  A single atlas object (class \`"atlas"\`) or a named list of atlas
  objects.

- radius:

  Numeric scalar giving the search radius in millimetres. \`0\`
  (default) performs an exact voxel lookup; values greater than zero
  return every region whose labelled voxels fall within \`radius\` mm of
  the query coordinate.

- from_space:

  Character string identifying the coordinate space of the input
  coordinates (default \`"MNI152"\`).

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- point:

  Integer index of the query coordinate (row number).

- x, y, z:

  The input world coordinates.

- atlas_name:

  Name of the atlas.

- id:

  Integer region ID (\`NA\` for background or out-of-bounds).

- label:

  Region label string (\`NA\` for background/OOB).

- hemi:

  Hemisphere designation (\`NA\` if unavailable).

- network:

  Network label (\`NA\` if unavailable).

## Details

World coordinates are converted to voxel grid positions via
[`coord_to_grid`](https://bbuchsbaum.github.io/neuroim2/reference/coord_to_grid-methods.html)
and rounded to the nearest integer. When \`radius \> 0\`, a
nearest-neighbour search is performed in world coordinates using
[`nn`](https://rdrr.io/pkg/Rnanoflann/man/nn.html) to find all labelled
atlas voxels within \`radius\` mm.

If the atlas carries a coordinate-space annotation
(\`atlas\$atlas_ref\$coord_space\`) that differs from \`from_space\`,
the input coordinates are transformed automatically via
[`transform_coords`](transform_coords.md).

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
query_point(c(24, -10, 5), atlas)

# Multiple points
pts <- rbind(c(24, -10, 5), c(-30, 20, 50))
query_point(pts, atlas)

# Fuzzy search within 5 mm
query_point(c(24, -10, 5), atlas, radius = 5)

# Query multiple atlases at once
atlases <- list(schaefer = atlas, aseg = get_aseg_atlas())
query_point(c(24, -10, 5), atlases)
} # }
```

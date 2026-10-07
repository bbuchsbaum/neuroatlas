# Identify an Exact Surface Vertex Domain

Constructs a descriptor from an ordered registration sphere, its
triangles, and its cortical mask. Template names and vertex counts alone
cannot establish correspondence. This function records identity; it does
not establish that a registration is anatomically valid or qualify a
resampling method. Array validation does not establish closed-manifold
topology or mesh quality.

## Usage

``` r
surface_domain(
  template,
  hemisphere,
  density,
  sphere,
  triangles,
  cortex,
  registration,
  revision,
  vertex_area = NULL,
  index_base = c("zero", "one")
)
```

## Arguments

- template:

  Exact template family identifier, such as \`"fsaverage"\` or
  \`"fsLR"\`. Aliases are not inferred.

- hemisphere:

  \`"L"\` or \`"R"\`.

- density:

  Density identifier, such as \`"164k"\` or \`"32k"\`.

- sphere:

  Numeric matrix of sphere coordinates, one vertex per row and three
  columns. Coordinates must be finite, nonzero, and approximately
  equidistant from the origin (relative radius tolerance 0.001).

- triangles:

  Integer-valued matrix with three vertex indices per row.

- cortex:

  Logical vector in vertex order; \`TRUE\` includes cortical vertices,
  \`FALSE\` excludes the medial wall. Missing values are disallowed.

- registration:

  Exact identifier for the sphere correspondence frame. Matching strings
  record a declaration, not proof of correspondence.

- revision:

  Immutable upstream sphere revision or content identifier.

- vertex_area:

  Optional positive, finite per-vertex areas in square mm, in the same
  vertex order. Absence is recorded explicitly.

- index_base:

  Triangle indexing: \`"zero"\` (GIFTI default) or \`"one"\` (R). It is
  never inferred from the minimum index.

## Value

A \`SurfaceDomain\` descriptor containing counts, metadata, SHA-256
fingerprints of ordered coordinates, topology, mask, and optional areas,
and a combined \`id\`. Arrays are not retained. Numeric arrays are
hashed as row-major little-endian doubles, indices as zero-based 32-bit
integers, and masks as 32-bit integers. Row names and R storage mode do
not affect identity; reordering vertices or triangles does. This
conservative identity deliberately distinguishes equivalent meshes with
different encodings.

## See also

\[atlas_transform_plan()\]

## Examples

``` r
sphere <- diag(3)
faces <- matrix(c(0L, 1L, 2L), nrow = 1)
cortex <- rep(TRUE, 3)
domain <- surface_domain(
  "toy", "L", "3v", sphere, faces, cortex,
  "analytic", "v1"
)
domain
#> $schema
#> [1] "neuroatlas.surface-domain.v1"
#> 
#> $template
#> [1] "toy"
#> 
#> $hemisphere
#> [1] "L"
#> 
#> $density
#> [1] "3v"
#> 
#> $registration
#> [1] "analytic"
#> 
#> $revision
#> [1] "v1"
#> 
#> $n_vertices
#> [1] 3
#> 
#> $n_triangles
#> [1] 1
#> 
#> $coordinates_sha256
#> [1] "1332df685c45a61a0ac008a6a653295b8c5655361c1d34b6304b22ec40235fb8"
#> 
#> $topology_sha256
#> [1] "ad5dc1478de06a4c2728ea528bd9361a4b945e92a414bf4d180cedaaeaa5f4cc"
#> 
#> $cortex_sha256
#> [1] "11047585fe102fbb5cadb42446612a578d88c6ef5ed076bb7ac360c4f9e4373d"
#> 
#> $area_sha256
#> [1] NA
#> 
#> $area_units
#> [1] NA
#> 
#> $id
#> [1] "06ee92dcd44a0f3b21c6f037805a5254ee1a0b938ea55abe1b97705609417059"
#> 
#> attr(,"class")
#> [1] "SurfaceDomain" "list"         
```

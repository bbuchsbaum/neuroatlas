# Bind Registration Geometry to a Surface Domain

Checks the ordered sphere, topology and cortical mask against an exact
\[surface_domain()\] descriptor. This does not estimate a registration.

## Usage

``` r
surface_geometry(
  domain,
  sphere,
  triangles = NULL,
  cortex,
  index_base = c("zero", "one")
)
```

## Arguments

- domain:

  A \`SurfaceDomain\` descriptor.

- sphere:

  Numeric vertex-by-three matrix, or a GIFTI surface filename.

- triangles:

  Triangle matrix; omit when reading a GIFTI surface.

- cortex:

  Logical cortical inclusion vector, or a binary GIFTI filename.

- index_base:

  Triangle indexing for matrices, \`"zero"\` or \`"one"\`. GIFTI
  triangles are always zero-based.

## Value

A \`SurfaceGeometry\` containing the verified domain and arrays.

## Examples

``` r
sphere <- diag(3)
faces <- matrix(c(0L, 1L, 2L), nrow = 1)
cortex <- rep(TRUE, 3)
domain <- surface_domain(
  "toy", "L", "3v", sphere, faces, cortex,
  "analytic", "v1"
)
surface_geometry(domain, sphere, faces, cortex)
#> $domain
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
#> 
#> $sphere
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
#> 
#> $triangles
#>      [,1] [,2] [,3]
#> [1,]    0    1    2
#> 
#> $cortex
#> [1] TRUE TRUE TRUE
#> 
#> $files
#> list()
#> 
#> attr(,"class")
#> [1] "SurfaceGeometry" "list"           
```

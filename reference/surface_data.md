# Bind Vertex Values to an Exact Surface Domain

Explicitly declaring the domain binds the caller's vertex ordering.
GIFTI hemisphere metadata, when present, must agree. A GIFTI file alone
cannot establish topology or vertex order; the caller must know its
domain.

## Usage

``` r
surface_data(
  values,
  domain,
  data_type = c("continuous", "label", "probability"),
  label_table = NULL
)
```

## Arguments

- values:

  Numeric vertex vector or vertex-by-map matrix, or a metric or label
  GIFTI filename. \`NA\` values represent missing data; infinity is
  rejected.

- domain:

  A \`SurfaceDomain\` descriptor.

- data_type:

  \`"continuous"\`, \`"label"\`, or \`"probability"\`. Probability
  values must lie in \[0,1\]; channels are never renormalized across
  maps.

- label_table:

  Optional data frame with unique integer \`key\` values and optional
  names/colors. Required when importing label GIFTI; read from the file
  when omitted. Every finite input key must appear in the table.

## Value

\`SurfaceData\`, preserving values, domain, label table and input
identity.

## Examples

``` r
sphere <- diag(3)
faces <- matrix(c(0L, 1L, 2L), nrow = 1)
cortex <- rep(TRUE, 3)
domain <- surface_domain(
  "toy", "L", "3v", sphere, faces, cortex,
  "analytic", "v1"
)
surface_data(c(0, 0.25, 0.5), domain)
#> $values
#> [1] 0.00 0.25 0.50
#> 
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
#> $data_type
#> [1] "continuous"
#> 
#> $label_table
#> NULL
#> 
#> $file
#> NULL
#> 
#> $id
#> [1] "eff3f728ad9eebad8bc7da93915af8e8b28c4ac5d238daa5d95e02dfd57cadc3"
#> 
#> attr(,"class")
#> [1] "SurfaceData" "list"       
```

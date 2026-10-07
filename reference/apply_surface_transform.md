# Apply a Domain-Bound Surface Transform

Continuous maps use row-normalized interpolation. Label keys use
categorical voting and are never averaged. Unsupported and target-masked
outputs are \`NA\`; supported zero, including label key zero, remains a
valid value. Missingness and source weight mass are returned separately
for every map. Probability output roundoff within \`1e-12\` of \[0,1\]
is clipped to that interval; larger excursions are errors. Input bounds
remain strict and probability channels are never renormalized across
maps.

## Usage

``` r
apply_surface_transform(
  x,
  transform,
  na_policy = c("propagate", "omit", "error"),
  label_method = c("aggregate", "largest")
)
```

## Arguments

- x:

  A \`SurfaceData\` object bound to the transform's exact source domain.

- transform:

  A verified \`SurfaceTransform\`.

- na_policy:

  \`"propagate"\`, \`"omit"\` with finite-weight renormalization, or
  \`"error"\`. Omission changes the effective operator per map.

- label_method:

  \`"aggregate"\` votes by total weight per key; ties select the
  smallest key. \`"largest"\` selects the largest-weight source vertex;
  ties select the smallest source index. Used only for categorical data.

## Value

\`SurfaceData\` on the target domain, with \`coverage\` diagnostics and
provenance including input identity, operator identity and policies.

## Examples

``` r
sphere <- rbind(
  c(1, 0, 0), c(-1, 0, 0), c(0, 1, 0),
  c(0, -1, 0), c(0, 0, 1), c(0, 0, -1)
)
faces <- rbind(
  c(0, 2, 4), c(2, 1, 4), c(1, 3, 4), c(3, 0, 4),
  c(2, 0, 5), c(1, 2, 5), c(3, 1, 5), c(0, 3, 5)
)
cortex <- rep(TRUE, 6)
domain <- surface_domain(
  "toy", "L", "6v", sphere, faces, cortex,
  "analytic", "v1"
)
if (requireNamespace("neurotransform", quietly = TRUE) &&
  utils::packageVersion("neurotransform") >= "0.2.0") {
  geometry <- surface_geometry(domain, sphere, faces, cortex)
  operator <- get_surface_transform(geometry, geometry, cache_dir = NULL)
  values <- surface_data(seq(0, 0.5, length.out = 6), domain)
  apply_surface_transform(values, operator)
}
#> $values
#> [1] 0.0 0.1 0.2 0.3 0.4 0.5
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
#> [1] "6v"
#> 
#> $registration
#> [1] "analytic"
#> 
#> $revision
#> [1] "v1"
#> 
#> $n_vertices
#> [1] 6
#> 
#> $n_triangles
#> [1] 8
#> 
#> $coordinates_sha256
#> [1] "397b9cf5c341e2ab2a6cac4be2113bab2fe1329f6d85db8bd83ac88f6b98c0f7"
#> 
#> $topology_sha256
#> [1] "9b68f7388ae56d60f95297f4dc3acaff4352cb445df75a71555daee5fcfab712"
#> 
#> $cortex_sha256
#> [1] "9638834d1f629557d9b80fa7eeb64409e238c0ef2e3149a55a39c3796f66e19a"
#> 
#> $area_sha256
#> [1] NA
#> 
#> $area_units
#> [1] NA
#> 
#> $id
#> [1] "0b1f7f5ff0576415cdc207ebda7867e85fa09e263ee08f9aba0351ed3d133836"
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
#> [1] "56995fe83dc37de98902604717cc55a6033362ed5fa42136023bd2447a1ef371"
#> 
#> $coverage
#> $coverage$support
#> [1] "triangle" "triangle" "triangle" "triangle" "triangle" "triangle"
#> 
#> $coverage$geometric_support
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $coverage$source_weight_mass
#> [1] 1 1 1 1 1 1
#> 
#> $coverage$finite_weight_mass
#> [1] 1 1 1 1 1 1
#> 
#> $coverage$target_mask
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $coverage$available
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE
#> 
#> $coverage$status
#> [1] "available" "available" "available" "available" "available" "available"
#> 
#> $coverage$effective_target_area
#> NULL
#> 
#> $coverage$source_roi_target_area
#> NULL
#> 
#> 
#> $provenance
#> $provenance$input_id
#> [1] "56995fe83dc37de98902604717cc55a6033362ed5fa42136023bd2447a1ef371"
#> 
#> $provenance$operator_id
#> [1] "184ac8709b163a0bbfc2fc3e674e9884002bcecf2abac6e03610bcc6997a91ff"
#> 
#> $provenance$specification
#> $provenance$specification$schema
#> [1] "neuroatlas.surface-transform.v1"
#> 
#> $provenance$specification$from
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
#> [1] "6v"
#> 
#> $registration
#> [1] "analytic"
#> 
#> $revision
#> [1] "v1"
#> 
#> $n_vertices
#> [1] 6
#> 
#> $n_triangles
#> [1] 8
#> 
#> $coordinates_sha256
#> [1] "397b9cf5c341e2ab2a6cac4be2113bab2fe1329f6d85db8bd83ac88f6b98c0f7"
#> 
#> $topology_sha256
#> [1] "9b68f7388ae56d60f95297f4dc3acaff4352cb445df75a71555daee5fcfab712"
#> 
#> $cortex_sha256
#> [1] "9638834d1f629557d9b80fa7eeb64409e238c0ef2e3149a55a39c3796f66e19a"
#> 
#> $area_sha256
#> [1] NA
#> 
#> $area_units
#> [1] NA
#> 
#> $id
#> [1] "0b1f7f5ff0576415cdc207ebda7867e85fa09e263ee08f9aba0351ed3d133836"
#> 
#> attr(,"class")
#> [1] "SurfaceDomain" "list"         
#> 
#> $provenance$specification$to
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
#> [1] "6v"
#> 
#> $registration
#> [1] "analytic"
#> 
#> $revision
#> [1] "v1"
#> 
#> $n_vertices
#> [1] 6
#> 
#> $n_triangles
#> [1] 8
#> 
#> $coordinates_sha256
#> [1] "397b9cf5c341e2ab2a6cac4be2113bab2fe1329f6d85db8bd83ac88f6b98c0f7"
#> 
#> $topology_sha256
#> [1] "9b68f7388ae56d60f95297f4dc3acaff4352cb445df75a71555daee5fcfab712"
#> 
#> $cortex_sha256
#> [1] "9638834d1f629557d9b80fa7eeb64409e238c0ef2e3149a55a39c3796f66e19a"
#> 
#> $area_sha256
#> [1] NA
#> 
#> $area_units
#> [1] NA
#> 
#> $id
#> [1] "0b1f7f5ff0576415cdc207ebda7867e85fa09e263ee08f9aba0351ed3d133836"
#> 
#> attr(,"class")
#> [1] "SurfaceDomain" "list"         
#> 
#> $provenance$specification$method
#> [1] "native_closest_barycentric"
#> 
#> $provenance$specification$radius
#> [1] 100
#> 
#> $provenance$specification$mask_policy
#> [1] "source_then_row_normalize_then_target"
#> 
#> $provenance$specification$engine
#> $provenance$specification$engine$package
#> [1] "neurotransform"
#> 
#> $provenance$specification$engine$version
#> [1] "0.2.0"
#> 
#> $provenance$specification$engine$source_sha
#> [1] "933edddda462593941e167726e8aaa7168ff103a"
#> 
#> $provenance$specification$engine$dll_sha256
#> [1] "acf2ec330c3f71bf717a772883b87c75159940a24890f1afc3d5c2e9d4d6065b"
#> 
#> $provenance$specification$engine$r_code_sha256
#> [1] "e7c6c612653f3ced29decc3c6dca5c923a52ca23dcac998a4195cc56c38201bb"
#> 
#> 
#> $provenance$specification$qualification
#> [1] "unqualified"
#> 
#> $provenance$specification$qualification_scope
#> NULL
#> 
#> $provenance$specification$route_id
#> NULL
#> 
#> $provenance$specification$reversible
#> [1] FALSE
#> 
#> 
#> $provenance$na_policy
#> [1] "propagate"
#> 
#> $provenance$label_method
#> NULL
#> 
#> 
#> attr(,"class")
#> [1] "SurfaceData" "list"       
```

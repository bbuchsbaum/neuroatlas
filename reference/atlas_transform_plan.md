# Plan a Transform Between Spaces

Computes a transform route between spaces using the packaged transform
registry.

## Usage

``` r
atlas_transform_plan(
  from_space,
  to_space,
  data_type = c("parcel", "vertex", "voxel"),
  mode = c("auto", "strict"),
  available_only = FALSE,
  provider = c("auto", "neuroatlas", "templateflow")
)
```

## Arguments

- from_space:

  Source space identifier or a \[surface_domain()\] descriptor.

- to_space:

  Target space identifier or a \[surface_domain()\] descriptor. Supply
  descriptors for both endpoints for native surface resampling, or an
  exact volume-template identifier and target descriptor for projection.
  Only identical domain descriptors establish surface identity. Named
  surface routes remain advisory: template names do not bind exact
  meshes or methods.

- data_type:

  Data type being transformed (\`"parcel"\`, \`"vertex"\`, \`"voxel"\`).
  Voxel routes stay in volumes; vertex routes stay on surfaces. Parcel
  planning can also describe a single directed projection. Projection
  steps are not automatically composed with other routes.

- mode:

  Planning mode. \`"auto"\` returns \`NULL\` if no route exists,
  \`"strict"\` errors.

- available_only:

  Restrict routing to available edges. The default also shows planned
  routes for diagnostic compatibility. Execution always uses available,
  executable edges only; retired edges are never selected.

- provider:

  Provider filter, \`"auto"\`, \`"neuroatlas"\`, or \`"templateflow"\`.

## Value

A list of class \`"atlas_transform_plan"\` with fields: \`from_space\`,
\`to_space\`, \`steps\`, \`n_steps\`, \`status\`, \`confidence\`, and
\`warnings\`.

\`steps\` is a data frame with one row per transform step and registry
columns. In \`mode = "auto"\`, returns \`NULL\` (with warning) if no
route exists.

## Details

Space identifiers are normalized internally, so aliases such as
\`"fslr32k"\` are accepted.

## Examples

``` r
# Direct route
p1 <- atlas_transform_plan("MNI305", "MNI152")

# Alias normalization + planned route
p2 <- atlas_transform_plan("fsaverage", "fslr32k")
p2$status
#> [1] "planned"
```

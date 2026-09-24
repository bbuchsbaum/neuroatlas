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

  Source space identifier.

- to_space:

  Target space identifier.

- data_type:

  Data type being transformed (\`"parcel"\`, \`"vertex"\`, \`"voxel"\`).
  Used for advisory warnings.

- mode:

  Planning mode. \`"auto"\` returns \`NULL\` if no route exists,
  \`"strict"\` errors.

- available_only:

  Restrict routing to available edges. The default also shows planned
  routes for diagnostic compatibility. Execution always uses available
  edges only; retired edges are never selected.

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

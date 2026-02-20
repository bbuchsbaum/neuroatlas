# Plan a Transform Between Spaces

Computes a direct or two-hop transform plan between spaces using the
packaged transform registry.

## Usage

``` r
atlas_transform_plan(
  from_space,
  to_space,
  data_type = c("parcel", "vertex", "voxel"),
  mode = c("auto", "strict")
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

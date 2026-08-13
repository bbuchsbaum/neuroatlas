# FSL Atlas Loaders

Load FSL-distributed atlases into the standard neuroatlas \`atlas\`
object shape. FSL atlases are described by XML files under
\`\$FSLDIR/data/atlases\`; many probabilistic atlases provide both a 4D
probability image and a 3D maximum-probability summary image. The
summary image labels are offset by one relative to
XML/probability-volume indices, so \`get_fsl_atlas()\` applies that
correction when loading max-probability summaries.

## Usage

``` r
get_fsl_atlas(
  name,
  fsl_dir = Sys.getenv("FSLDIR"),
  resolution = NULL,
  image = c("summary", "probability"),
  outspace = NULL,
  path_only = FALSE
)
```

## Arguments

- name:

  Atlas identifier, FSL XML path, or known alias. Known aliases include
  \`"harvard_oxford_cortical"\`, \`"harvard_oxford_subcortical"\`,
  \`"harvard_oxford_cortical_subcortical"\`, and \`"julich"\`.

- fsl_dir:

  FSL installation directory. Defaults to \`Sys.getenv("FSLDIR")\`.
  \`get_julich_brain_atlas()\` downloads an FSL-style Julich-Brain cache
  when this is empty and \`download = TRUE\`.

- resolution:

  Preferred image resolution, e.g. \`"1mm"\` or \`"2mm"\`. If \`NULL\`,
  the first image entry in the XML file is used.

- image:

  One of \`"summary"\` or \`"probability"\`. \`neuroatlas\` atlas
  objects are discrete parcellations, so \`"summary"\` is the default.
  \`"probability"\` currently returns paths and metadata when
  \`path_only = TRUE\`; loading 4D probabilistic images as atlas objects
  is intentionally deferred.

- outspace:

  Optional \`NeuroSpace\` to resample the discrete atlas into.

- path_only:

  Logical; return resolved paths and parsed metadata without loading
  image data.

## Value

An \`atlas\` object, or a path/metadata list when \`path_only = TRUE\`.

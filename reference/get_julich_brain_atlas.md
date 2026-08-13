# Load a Julich-Brain FSL Atlas

Thin wrapper around \`get_fsl_atlas()\` for the FSL-distributed
Julich-Brain cytoarchitectonic atlas. This requires a local FSL-style
atlas directory and uses the XML/image files under
\`\$FSLDIR/data/atlases\`.

## Usage

``` r
get_julich_brain_atlas(fsl_dir = Sys.getenv("FSLDIR"), download = TRUE, ...)
```

## Arguments

- fsl_dir:

  FSL installation directory. Defaults to \`Sys.getenv("FSLDIR")\`.
  \`get_julich_brain_atlas()\` downloads an FSL-style Julich-Brain cache
  when this is empty and \`download = TRUE\`.

## Value

An \`atlas\` object, or path metadata when \`path_only = TRUE\`.

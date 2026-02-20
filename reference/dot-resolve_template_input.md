# Resolve Template Input to NeuroVol or NeuroSpace

This internal helper function takes a flexible input representing a
neuroimaging template and resolves it to either a \`neuroim2::NeuroVol\`
object or a \`neuroim2::NeuroSpace\` object, typically by fetching it
via \`get_template()\` if it's not already in the desired R object form.

## Usage

``` r
.resolve_template_input(input, target_type = "NeuroVol", api_handle = NULL)
```

## Arguments

- input:

  The input to resolve. Can be: - A \`neuroim2::NeuroVol\` object. - A
  \`neuroim2::NeuroSpace\` object. - A character string: Assumed to be a
  TemplateFlow \`space\` identifier. \`get_template()\` will be called
  with this space and default values for other parameters (e.g.,
  \`variant="brain"\`, \`resolution="1"\`). - A named list: Assumed to
  be arguments for \`get_template()\`. \`do.call(get_template, input)\`
  will be used.

- target_type:

  A character string, either "NeuroVol" (default) or "NeuroSpace",
  specifying the desired output type.

- api_handle:

  (Optional) An existing \`templateflow\` S3 object.

## Value

An object of the \`target_type\`. If \`target_type\` is "NeuroSpace" and
a \`NeuroVol\` is obtained, its space is extracted via
\`neuroim2::space()\`. Returns \`NULL\` or stops on error if resolution
fails.

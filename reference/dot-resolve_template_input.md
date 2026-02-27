# Resolve Template Input to NeuroVol or NeuroSpace

Internal helper function that takes a flexible input representing a
neuroimaging template and resolves it to either a `NeuroVol` or
`NeuroSpace` object.

## Usage

``` r
.resolve_template_input(input, target_type = "NeuroVol", api_handle = NULL)
```

## Arguments

- input:

  The input to resolve. Can be a `NeuroVol`, `NeuroSpace`, a
  TemplateFlow space string, or a named list of
  [`get_template()`](get_template.md) arguments.

- target_type:

  "NeuroVol" (default) or "NeuroSpace".

- api_handle:

  Deprecated and ignored.

## Value

An object of the specified `target_type`.

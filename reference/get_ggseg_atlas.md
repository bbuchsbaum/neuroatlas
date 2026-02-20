# Get ggseg-Compatible Schaefer Atlas

\`r lifecycle::badge("deprecated")\`

This function has been deprecated in favour of the native
[`plot_brain()`](plot_brain.md) renderer. Use
[`schaefer_surf()`](schaefer_surf.md) to obtain a surface atlas and
[`plot_brain()`](plot_brain.md) for visualisation.

## Usage

``` r
get_ggseg_atlas(atlas)
```

## Arguments

- atlas:

  An atlas object containing Schaefer parcellation information.

## Value

A ggseg brain atlas object for visualization (if ggsegSchaefer is
installed).

## Examples

``` r
# \donttest{
# Deprecated — use plot_brain(schaefer_surf(200, 17)) instead
# }
```

# Create Interactive Schaefer Atlas Visualization

\`r lifecycle::badge("deprecated")\`

This function has been deprecated in favour of
[`plot_brain()`](plot_brain.md). Use `plot_brain(schaefer_surf(...))`
for interactive cortical surface visualisation.

## Usage

``` r
ggseg_schaefer(
  atlas,
  vals,
  thresh = NULL,
  pos = FALSE,
  palette = "Spectral",
  interactive = TRUE,
  lim = range(vals)
)
```

## Arguments

- atlas:

  An atlas object containing Schaefer parcellation information

- vals:

  Numeric vector of values to visualize on the atlas

- thresh:

  Numeric vector of length 2 specifying (min, max) thresholds.

- pos:

  Logical. If TRUE, uses raw values for thresholding.

- palette:

  Character string specifying the color palette. Default: "Spectral"

- interactive:

  Logical. If TRUE, creates an interactive plot. Default: TRUE

- lim:

  Numeric vector of length 2 specifying the range for color mapping.

## Value

A ggplot2 or ggiraph object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Deprecated — use plot_brain(schaefer_surf(200, 17), vals = ...) instead
} # }
```

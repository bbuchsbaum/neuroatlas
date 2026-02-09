# Plot Glasser Atlas Values

\`r lifecycle::badge("deprecated")\`

This function has been deprecated. Use
`plot_brain(glasser_surf(), vals = ...)` instead for interactive
cortical surface visualisation.

## Usage

``` r
plot_glasser(vals = NULL, value_col = "value", position = "dispersed")
```

## Arguments

- vals:

  A data frame containing values to plot, must include columns:

  - label: character, matching ggseg Glasser atlas labels

  - value: numeric, values to visualize for each region

  If NULL (default), all regions will be assigned a value of 1

- value_col:

  Character string specifying the name of the column in vals containing
  the values to plot. Defaults to "value"

- position:

  Character string specifying layout type.

## Value

An echarts4r visualization object or an error if dependencies are
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
# Deprecated — use plot_brain(glasser_surf(), vals = ...) instead
} # }
```

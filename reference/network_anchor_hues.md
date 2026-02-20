# Assign harmonic anchor hues to networks

Assign harmonic anchor hues to networks

## Usage

``` r
network_anchor_hues(
  network_levels,
  scheme = c("even", "triadic", "tetradic", "complementary"),
  start_hue = 15
)
```

## Arguments

- network_levels:

  Character vector of unique network names.

- scheme:

  Hue spacing scheme: \`"even"\`, \`"triadic"\`, \`"tetradic"\`, or
  \`"complementary"\`.

- start_hue:

  Starting hue in degrees.

## Value

Named numeric vector of anchor hues.

## Examples

``` r
hues <- network_anchor_hues(c("Visual", "Default", "DorsalAttn"))
hues
#>     Visual    Default DorsalAttn 
#>         15        135        255 
```

# Filter Atlas with Infix Operator

A concise infix operator for filtering atlas regions by metadata
attributes. Equivalent to calling [`filter_atlas`](filter_atlas.md) but
allows a more fluent pipe-friendly syntax.

## Usage

``` r
atlas %where% expr
```

## Arguments

- atlas:

  An atlas object.

- expr:

  An unquoted filter expression using column names from
  [`roi_metadata`](roi_metadata.md). Multiple conditions can be combined
  with `&` and `|`.

## Value

A new atlas object containing only the matching ROIs.

## See also

[`filter_atlas`](filter_atlas.md) for the underlying function,
[`roi_metadata`](roi_metadata.md) for available filter columns

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")

# Filter to left-hemisphere Default network
sub <- atlas %where% (network == "Default" & hemi == "left")

# Filter by label pattern
vis <- atlas %where% grepl("Vis", label)
} # }
```

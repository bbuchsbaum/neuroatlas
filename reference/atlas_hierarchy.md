# Atlas Hierarchy

Extracts hierarchical level information from an atlas, describing how
parcels map to higher-level groupings (networks, hemispheres).

## Usage

``` r
atlas_hierarchy(atlas)
```

## Arguments

- atlas:

  An atlas object.

## Value

A list of class `"atlas_hierarchy"` with components:

- levels:

  Character vector of hierarchy level names, from finest to coarsest
  (e.g., `c("parcel", "network", "hemisphere")`).

- mappings:

  Named list of named character vectors. Each element maps parcel labels
  to the corresponding grouping at that level. Names of the list
  correspond to `levels[-1]` (all levels above parcel).

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
h <- atlas_hierarchy(atlas)
h$levels
# [1] "parcel" "network" "hemisphere"
h$mappings$network[1:5]
} # }
```

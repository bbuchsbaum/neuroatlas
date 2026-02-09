# ROI Metadata Functions

Functions for accessing and filtering ROI (Region of Interest) metadata
in atlas objects. These functions provide a unified, discoverable
interface for working with multiple ROI attributes across different
atlas types.

Returns a tibble containing metadata for all regions of interest (ROIs)
in an atlas. This provides a unified, tidy interface for accessing ROI
attributes across different atlas types.

## Usage

``` r
roi_metadata(x, ...)

# S3 method for class 'atlas'
roi_metadata(x, ...)
```

## Arguments

- x:

  An atlas object

- ...:

  Additional arguments passed to methods

## Value

A tibble with one row per ROI and columns for each attribute. Standard
columns include:

- id:

  Numeric ROI identifier

- label:

  Simplified region name

- label_full:

  Original/full region label

- hemi:

  Hemisphere ("left", "right", or NA for bilateral/midline)

- color_r, color_g, color_b:

  RGB color values (0-255)

Additional atlas-specific columns may be present (e.g., `network` for
Schaefer atlases).

## See also

[`roi_attributes`](roi_attributes.md) for listing available attributes,
[`filter_atlas`](filter_atlas.md) for filtering atlas objects by
attributes

## Examples

``` r
if (FALSE) { # \dontrun{
# Get metadata for Schaefer atlas
atlas <- get_schaefer_atlas(parcels = "200", networks = "7")
meta <- roi_metadata(atlas)

# Filter using dplyr
library(dplyr)
left_visual <- meta %>% filter(hemi == "left", network == "Vis")
} # }
```

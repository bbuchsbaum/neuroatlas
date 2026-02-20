# Load Schaefer Brain Parcellation Atlas

Retrieves and loads the Schaefer brain parcellation atlas, which
provides a data-driven parcellation of the cerebral cortex based on both
local gradient and global similarity approaches.

## Usage

``` r
get_schaefer_atlas(
  parcels = c("100", "200", "300", "400", "500", "600", "700", "800", "900", "1000"),
  networks = c("7", "17"),
  resolution = c("1", "2"),
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE
)

sy_100_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_100_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_200_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_200_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_300_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_300_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_400_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_400_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_500_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_500_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_600_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_600_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_700_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_700_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_800_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_800_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_900_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_900_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_1000_7(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)

sy_1000_17(
  resolution = "2",
  outspace = NULL,
  smooth = FALSE,
  use_cache = TRUE,
  ...
)
```

## Source

<https://github.com/ThomasYeoLab/CBIG/>

## Arguments

- parcels:

  Character string specifying number of parcels. Options: "100", "200",
  "300", "400", "500", "600", "800", "1000"

- networks:

  Character string specifying network count. Options: "7", "17"

- resolution:

  Character string specifying MNI space resolution in mm. Options: "1",
  "2"

- outspace:

  Optional `NeuroSpace` object for resampling the atlas

- smooth:

  Logical. Whether to smooth parcel boundaries after resampling.
  Default: FALSE

- use_cache:

  Logical. Whether to cache downloaded files. Default: TRUE

- ...:

  Additional arguments (currently unused, included for consistency with
  convenience functions)

## Value

A list with classes c("schaefer", "volatlas", "atlas") containing:

- name:

  Character string identifying atlas version

- atlas:

  `ClusteredNeuroVol` object containing the parcellation

- cmap:

  Data frame with RGB colors for visualization

- ids:

  Integer vector of region IDs

- labels:

  Character vector of region names

- orig_labels:

  Original region labels from source data

- network:

  Network assignment for each region

- hemi:

  Hemisphere designation for each region

## Details

The Schaefer atlas offers multiple resolutions of cortical parcellation
(100-1000 parcels) and two network versions (7 or 17 networks). The
atlas is based on resting-state functional connectivity from 1489
subjects. Features include:

- Multiple granularity levels (100-1000 parcels)

- Network assignments (7 or 17 networks)

- Bilateral parcellation

- Available in different resolutions (1mm or 2mm)

## Convenience Functions

Shorthand functions are provided for common Schaefer atlas
configurations. These functions call `get_schaefer_atlas` with the
`parcels` and `networks` arguments pre-set. They all accept `resolution`
(default "2"), `outspace`, `smooth`, `use_cache`, and `...` arguments.

- `sy_100_7()`: 100 parcels, 7 networks.

- `sy_100_17()`: 100 parcels, 17 networks.

- `sy_200_7()`: 200 parcels, 7 networks.

- `sy_200_17()`: 200 parcels, 17 networks.

- `sy_300_7()`: 300 parcels, 7 networks.

- `sy_300_17()`: 300 parcels, 17 networks.

- `sy_400_7()`: 400 parcels, 7 networks.

- `sy_400_17()`: 400 parcels, 17 networks.

- `sy_500_7()`: 500 parcels, 7 networks.

- `sy_500_17()`: 500 parcels, 17 networks.

- `sy_600_7()`: 600 parcels, 7 networks.

- `sy_600_17()`: 600 parcels, 17 networks.

- `sy_800_7()`: 800 parcels, 7 networks.

- `sy_800_17()`: 800 parcels, 17 networks.

- `sy_1000_7()`: 1000 parcels, 7 networks.

- `sy_1000_17()`: 1000 parcels, 17 networks.

## References

Schaefer, A., et al. (2018). Local-Global Parcellation of the Human
Cerebral Cortex from Intrinsic Functional Connectivity MRI. Cerebral
Cortex, 28(9), 3095-3114.

## See also

[`get_schaefer_surfatlas`](get_schaefer_surfatlas.md) for surface-based
version

## Examples

``` r
if (FALSE) { # \dontrun{
# Load 300-parcel atlas with 7 networks
atlas <- get_schaefer_atlas(parcels = "300", networks = "7")

# Load high-resolution version
atlas_hires <- get_schaefer_atlas(parcels = "400",
                                 networks = "17",
                                 resolution = "1")

# Resample to a different space
new_space <- neuroim2::NeuroSpace(dim = c(91,109,91),
                                 spacing = c(2,2,2))
atlas_resampled <- get_schaefer_atlas(parcels = "300",
                                     outspace = new_space)
} # }
```

# Load Surface-Based Schaefer Atlas

Loads the surface-based version of the Schaefer parcellation atlas,
compatible with FreeSurfer surface representations.

## Usage

``` r
get_schaefer_surfatlas(
  parcels = c("100", "200", "300", "400", "500", "600", "800", "1000"),
  networks = c("7", "17"),
  surf = c("inflated", "white", "pial"),
  use_cache = TRUE
)
```

## Arguments

- parcels:

  Character string specifying number of parcels. Options: "100", "200",
  "300", "400", "500", "600", "800", "1000"

- networks:

  Character string specifying network count. Options: "7", "17"

- surf:

  Character string specifying surface type. Options: "inflated",
  "white", "pial"

- use_cache:

  Logical. Whether to cache downloaded files. Default: TRUE

## Value

A list with classes c("schaefer", "surfatlas", "atlas") containing:

- surf_type:

  Surface type used

- lh_atlas:

  Left hemisphere surface atlas

- rh_atlas:

  Right hemisphere surface atlas

- name:

  Atlas identifier

- cmap:

  RGB color specifications

- ids:

  Region IDs

- labels:

  Region names

- orig_labels:

  Original region labels

- network:

  Network assignments

- hemi:

  Hemisphere designations

## Details

Provides the Schaefer parcellation mapped to FreeSurfer surface meshes.
The atlas can be loaded onto different surface representations
(inflated, white, or pial) and maintains the same parcellation scheme as
the volumetric version.

## See also

[`get_schaefer_atlas`](get_schaefer_atlas.md) for volumetric version

## Examples

``` r
if (FALSE) { # \dontrun{
# Load inflated surface atlas
surf_atlas <- get_schaefer_surfatlas(parcels = "300",
                                    networks = "7",
                                    surf = "inflated")

# Load pial surface version
pial_atlas <- get_schaefer_surfatlas(parcels = "400",
                                    networks = "17",
                                    surf = "pial")
} # }
```

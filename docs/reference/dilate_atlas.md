# Dilate Atlas Parcellation Boundaries

Expands the boundaries of brain atlas parcels by dilating them into
adjacent unassigned voxels within a specified mask. This is useful for
filling small gaps between parcels or extending parcels into neighboring
regions.

## Usage

``` r
dilate_atlas(atlas, mask, radius = 4, maxn = 50)
```

## Arguments

- atlas:

  An object of class "atlas" containing the parcellation to be dilated

- mask:

  A binary mask (NeuroVol object) specifying valid voxels for dilation.
  Dilation will only occur within non-zero mask values. May also be a
  TemplateFlow space identifier (string) or list of \`get_template()\`
  arguments, which will be resolved to a NeuroVol via
  \`.resolve_template_input()\`.

- radius:

  Numeric. The maximum distance (in voxels) to search for neighboring
  parcels when dilating. Default: 4

- maxn:

  Integer. Maximum number of neighboring voxels to consider when
  determining parcel assignment. Default: 50

## Value

A `ClusteredNeuroVol` object containing the dilated parcellation. The
object maintains the original label mappings but may include additional
voxels in existing parcels.

## Details

The dilation process:

- Identifies unassigned voxels within the mask that are adjacent to
  existing parcels

- For each unassigned voxel, finds nearby assigned voxels within the
  specified radius

- Assigns the unassigned voxel to the nearest parcel

- Respects mask boundaries to prevent dilation into unwanted regions

The function uses a k-d tree implementation (via Rnanoflann) for
efficient nearest neighbor searches in 3D space.

## References

The algorithm uses efficient k-d tree based nearest neighbor searches
for spatial queries in 3D voxel space.

## See also

[`get_template_brainmask`](get_template_brainmask.md) for creating
appropriate masks from TemplateFlow

## Examples

``` r
if (FALSE) { # \dontrun{
# Load an atlas
atlas <- get_aseg_atlas()

# Create or load a brain mask
mask <- get_template_brainmask()

# Dilate the atlas within the mask
dilated <- dilate_atlas(atlas, mask, radius = 4)

# More conservative dilation with fewer neighbors
dilated_conservative <- dilate_atlas(atlas, mask, radius = 2, maxn = 20)
} # }
```

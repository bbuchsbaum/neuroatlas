# neuroatlas: Neuroimaging Atlases and Parcellations

Provides a unified interface to access and work with various
neuroimaging atlases and parcellations including Schaefer, Glasser,
FreeSurfer ASEG, and Olsen MTL atlases. Integrates with TemplateFlow for
standardized template access and supports interactive brain surface
visualisation via triangle-mesh rendering with 'ggplot2' and 'ggiraph'.

The neuroatlas package provides a unified interface to access and work
with various neuroimaging atlases and parcellations. It includes support
for cortical atlases (Schaefer, Glasser), subcortical segmentations
(FreeSurfer ASEG), and specialized atlases (Olsen MTL). The package
integrates with TemplateFlow for standardized template access and
supports interactive brain surface visualisation via triangle-mesh
rendering.

## Main Functions

- [`get_schaefer_atlas`](get_schaefer_atlas.md):

  Access Schaefer cortical parcellations

- [`get_glasser_atlas`](get_glasser_atlas.md):

  Access Glasser multi-modal parcellation

- [`get_aseg_atlas`](get_aseg_atlas.md):

  Access FreeSurfer subcortical segmentation

- [`get_olsen_mtl`](get_olsen_mtl.md):

  Access Olsen medial temporal lobe atlas

- [`get_template`](get_template.md):

  Fetch templates from TemplateFlow

## Atlas Operations

- [`get_roi`](get_roi.md):

  Extract specific regions from an atlas

- [`map_atlas`](map_atlas.md):

  Map values to atlas regions

- [`reduce_atlas`](reduce_atlas.md):

  Combine regions within an atlas

- [`merge_atlases`](merge_atlases.md):

  Combine multiple atlases

- [`dilate_atlas`](dilate_atlas.md):

  Expand atlas regions into unassigned voxels

## Visualization

- [`plot_brain`](plot_brain.md):

  Interactive cortical surface rendering

- [`plot.atlas`](plot-methods.md):

  Plot volumetric atlas objects

- [`atlas_roi_colors`](atlas_roi_colors.md):

  Optimal ROI colour assignment

## TemplateFlow Integration

Access standardized neuroimaging templates:

- [`create_templateflow`](create_templateflow.md):

  Initialize TemplateFlow connection

- [`tflow_spaces`](tflow_spaces.md):

  List available template spaces

- [`install_templateflow`](install_templateflow.md):

  Install Python TemplateFlow module

## See also

Useful links:

- <https://github.com/bbuchsbaum/neuroatlas>

- <https://bbuchsbaum.github.io/neuroatlas/>

- Report bugs at <https://github.com/bbuchsbaum/neuroatlas/issues>

## Author

**Maintainer**: Bradley Buchsbaum <brad.buchsbaum@gmail.com>
([ORCID](https://orcid.org/0000-0000-0000-0000))

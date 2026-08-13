# neuroatlas: Neuroimaging Atlases and Parcellations

Provides a unified interface to access and work with various
neuroimaging atlases and parcellations including Schaefer, Brainnetome,
Glasser, FreeSurfer ASEG, and Olsen MTL atlases. Integrates with
TemplateFlow for standardized template access and supports interactive
brain surface visualisation via triangle-mesh rendering with 'ggplot2'
and 'ggiraph'.

The neuroatlas package provides a unified interface to access and work
with various neuroimaging atlases and parcellations. It includes support
for cortical atlases (Schaefer, Glasser), subcortical segmentations
(FreeSurfer ASEG), FSL/TemplateFlow structural atlases (Harvard-Oxford,
Julich-Brain), and specialized atlases (Olsen MTL). The package
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

- [`get_harvard_oxford_atlas`](get_harvard_oxford_atlas.md):

  Access Harvard-Oxford structural atlases

- [`get_fsl_atlas`](get_fsl_atlas.md):

  Access FSL XML-described atlases

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

Access standardized neuroimaging templates through the pure-R
\`templateflow\` backend:

- [`get_template`](get_template.md):

  Fetch a template asset

- [`tflow_spaces`](tflow_spaces.md):

  List available template spaces

- [`tflow_files`](tflow_files.md):

  Query template assets

## See also

Useful links:

- <https://github.com/bbuchsbaum/neuroatlas>

- <https://bbuchsbaum.github.io/neuroatlas/>

- Report bugs at <https://github.com/bbuchsbaum/neuroatlas/issues>

## Author

**Maintainer**: Bradley Buchsbaum <brad.buchsbaum@gmail.com>
([ORCID](https://orcid.org/0000-0002-1108-4866))

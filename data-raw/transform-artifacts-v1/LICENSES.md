# Transform artifacts and qualification fixtures

The neuroatlas R implementation and build scripts retain the repository's MIT
license. This notice concerns the separate template-derived transform artifacts
and their qualification evidence. It does not relicense upstream templates or
atlases under MIT.

## MNI152NLin6Asym

TemplateFlow identifies this template as FSL's asymmetric sixth-generation
MNI152 template. The frozen source images and masks come from that dataset.

- Dataset: <https://github.com/templateflow/tpl-MNI152NLin6Asym>
- Source description: <https://fsl.fmrib.ox.ac.uk/fsl/docs/other/datasets.html>
- Distribution terms: <https://fsl.fmrib.ox.ac.uk/fsl/docs/license.html>

FMRIB Software Library, Release 6.0 (c) 2018, The University of Oxford.

This artifact distribution is without financial return. Preserve this notice,
source attribution, and the linked upstream conditions when redistributing the
transform/evidence bundle. The FSL terms restrict commercial use; this release
does not grant additional commercial rights. Complete original and modified
neuroatlas build/qualification source is distributed in the source repository.
The upstream template images themselves are obtained separately through
TemplateFlow and are not bundled with the R package or transform release.

## MNI152NLin2009cAsym

Copyright (C) 1993-2004 Louis Collins, McConnell Brain Imaging Centre,
Montreal Neurological Institute, McGill University.

McGill permits use, copying, modification and distribution with its copyright
notice, and provides the material without warranty. The complete notice and
requested scientific references are at:
<https://www.mcgill.ca/bic/software/tools-data-analysis/anatomical-mri/atlases/icbm152-non-linear-2009>.

TemplateFlow source: <https://github.com/templateflow/tpl-MNI152NLin2009cAsym>.
Please cite Fonov et al., NeuroImage 54 (2011),
<https://doi.org/10.1016/j.neuroimage.2010.07.033>.

## Harvard-Oxford qualification labels

Harvard-Oxford cortical and subcortical labels are used only to select numerical
sampling regions and report concordance. The mapped target labels are not
independent anatomical ground truth. Credit the Harvard-Oxford atlas, its
contributors listed by FSL, and TemplateFlow for distribution and mapping.

The Harvard-Oxford atlas is distributed under CC BY-SA 4.0 according to the FSL
license page above: <https://creativecommons.org/licenses/by-sa/4.0/>.
Label-derived visual panels and tables in this evidence bundle retain those
attribution/share-alike terms. The numerical coordinate files record deterministic
subsets on native grids; the generator and exact input hashes are included.

## Software and scope

Registration uses the pinned ANTs container through niflowr. Independent
application uses neurotransform. Software versions, immutable commits, container
digests, input receipts and output checksums are recorded in provenance.

These are template-to-template image pullbacks evaluated on the declared grids.
They do not establish individual-subject accuracy or equivalence between region
labels belonging to different parcellations.

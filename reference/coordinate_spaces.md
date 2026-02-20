# Coordinate Space Transforms for Neuroimaging Templates

Functions and constants for transforming coordinates between standard
neuroimaging coordinate spaces, particularly MNI305 (fsaverage) and
MNI152 (common fMRI template space).

## Details

FreeSurfer's fsaverage surfaces are defined in MNI305 (Talairach-like)
space, while most modern fMRI pipelines output data in MNI152 space. The
difference is approximately 4mm, which matters for accurate
volume-to-surface projections.

## Coordinate Spaces

- MNI305:

  FreeSurfer/Talairach space. Native space for fsaverage, fsaverage5,
  and fsaverage6 surfaces. Based on 305 subjects with linear
  registration to approximate Talairach space.

- MNI152:

  ICBM 2009c space. The de facto standard for volumetric fMRI analysis.
  Used by FSL, SPM, fMRIPrep, and TemplateFlow's MNI152NLin2009cAsym
  template.

## References

FreeSurfer CoordinateSystems documentation:
<https://surfer.nmr.mgh.harvard.edu/fswiki/CoordinateSystems>

Wu et al. (2018). Accurate nonlinear mapping between MNI volumetric and
FreeSurfer surface coordinate systems. Human Brain Mapping, 39(9),
3793-3808. [doi:10.1002/hbm.24213](https://doi.org/10.1002/hbm.24213)

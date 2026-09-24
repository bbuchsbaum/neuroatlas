Synthetic compatibility probes copied byte-for-byte from neurotransform commit
9e7550d45d6d6b06155b27717057f4137aaf666e, inst/extdata/itk_oracle.
Generated independently with SimpleITK 2.5.6 (ITK 5.4), NumPy 2.5.3;
expected RAS points are SimpleITK TransformPoint outputs.
The two H5s contain noncommuting affine and multilinear displacement fields
on an oblique unequal-spacing grid. They detect historical H5 layout, affine
direction, RAS/LPS and component-order errors without clinical data.
These 21 KB files are synthetic engine probes, not distributed atlas warps.
Generation source: https://github.com/bbuchsbaum/neurotransform/blob/9e7550d45d6d6b06155b27717057f4137aaf666e/tools/generate_itk_oracle.py
affine_warp.h5 SHA256 6b1b05bf64b7c6a5b3c515bef70f8e61ed154f0791fd795d32a354930f8ba51e
warp_affine.h5 SHA256 bcd135d6ef4729171969a23ef0925dcb4475d247d09be8ad51de7295c0a06d4c

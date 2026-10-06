# Surface and cortical projection input licenses

Reviewed 2026-10-06. These inputs are downloaded from their providers and are
not bundled or relicensed under neuroatlas's MIT code license. Numerical route
qualification does not change data licensing. Keep upstream notices with cached
inputs and with any independently redistributed copies or derivatives.

| Input | Pinned provenance | Review and distribution decision |
| --- | --- | --- |
| fsaverage 164k sphere, area and anatomical surfaces | TemplateFlow `tpl-fsaverage` at `8e53ba4f2e438758f69d11436fe0cd291a28bec6`; checksums in `inputs.lock.json` | FreeSurfer origin. The pinned template description says "See LICENSE file", but its tree has no LICENSE. Preserve FreeSurfer attribution and applicable terms; download from upstream only. Do not infer CC0 from TemplateFlow hosting. |
| fsLR 32k sphere in fsaverage correspondence, mask, area and anatomy | TemplateFlow `tpl-fsLR` at `ca545b4721c2858decef9bbca302c1eac4d0d8bf` | HCP origin; the pinned description likewise refers to an absent LICENSE. HCP Pipelines has BSD-3-Clause terms and component exceptions; this does not by itself resolve the exact TemplateFlow files' complete rights chain. Download from upstream only. |
| fsaverage cortical masks and ordering references | neuromaps manifest `ffcc2e0f657943ce00a1b6a968396f32250e495c`, OSF archive SHA-256 `cab55f06116ce0c00f6ad00f93129a82174d0d693f7d9b64c93ba7d605fd1a16` | neuromaps repository license is CC BY-NC-SA 4.0. The OSF archive has no separate license notice. Conservatively retain those terms plus original FreeSurfer attribution; do not describe these masks as MIT/BSD/CC0. Download only; redistribution remains unresolved per asset. |
| CBIG RF-ANTs MNI152-original-to-fsaverage sampling coordinates, L/R | CBIG `634f676630929a71297852d01dd92a287103e861`, `stable_projects/registration/Wu2017_RegistrationFusion/bin/final_warps_FS5.3/*h.avgMapping_allSub_RF_ANTs_MNI152_orig_to_fsaverage.mat` | CBIG repository license is MIT with its copyright/permission notice retained. Use the original checksum-pinned upstream files; do not bundle the FSL/FreeSurfer reference volumes used to audit their coordinate frame. Retain the Wu et al. citation and original source URL in provenance. |
| FSL MNI152 reference volume and TemplateFlow MNI6 T1w/mask | CBIG `data/templates/volume/FSL_MNI152_FS4.5.0/mri/orig/001.mgz` and TemplateFlow MNI152NLin6Asym | Audit inputs only, not package/release payloads. FSL and template data terms are separate from CBIG's code license. The volumes differ outside the sampling support; the projection audit records that limitation. |
| MNI6/MNI2009c nonlinear transforms | Existing `transform-artifacts-v1` release | Its immutable artifact licenses and notices continue to apply; see `data-raw/transform-artifacts-v1/LICENSES.md`. |

Primary license and provenance sources:

- [FreeSurfer software/data license](https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense).
- [HCP Pipelines license and exceptions](https://github.com/Washington-University/HCPpipelines/blob/master/LICENSE.md).
- [Pinned neuromaps license](https://github.com/netneurolab/neuromaps/blob/ffcc2e0f657943ce00a1b6a968396f32250e495c/LICENSE).
- [Pinned CBIG license](https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/LICENSE.md).
- [CBIG registration fusion methods and source](https://github.com/ThomasYeoLab/CBIG/tree/634f676630929a71297852d01dd92a287103e861/stable_projects/registration/Wu2017_RegistrationFusion).
- [Wu et al. (2018)](https://doi.org/10.1002/hbm.24213).

The public cache stores original bytes under their SHA-256 identity. A cache
receipt records integrity and ownership, not a grant of permission. No raw
anatomical audit volume, mesh, cortical mask or sampling matrix from this pinned
input set is added to the neuroatlas package or proposed release. Exact-domain
route admission can proceed
using verified upstream inputs while unresolved redistribution stays explicit.

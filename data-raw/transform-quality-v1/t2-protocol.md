# Additional-contrast diagnostic, fixed before T2 measurements

2026-10-07. Use pinned TemplateFlow MNI6 res-04 T2w (0.7 mm; introduced
from HCP Pipelines in commit 40c0c1c4c5cd72d2b6c25237078899b3120de231)
and MNI2009c res-01 T2w. Sample native source T2 directly to the four
existing target T1 grids, using identity and released pullbacks with linear
interpolation. Evaluate correlation in the target brain and every HOCPA/HOSPA
region. Report worsening regions without selecting them after measurement.

This is an additional contrast, not independently paired anatomical truth.
The MNI atlas authors state that T1-derived registrations were also applied
to T2 to form the group templates. The full construction provenance of the
HCP-derived MNI6 T2 has not been established. Therefore even excellent T2
agreement cannot certify an independent anatomical error bound.

Reference: https://www.bic.mni.mcgill.ca/ServicesAtlases/ICBM152NLin2009

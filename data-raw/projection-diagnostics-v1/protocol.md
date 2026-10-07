# Fine-parcel projection diagnostics and method comparison

Frozen before this campaign's measurements, 7 October 2026. This supplements
the earlier quality campaign without changing its no-label-loss criterion or
the original release evidence. No method is admitted by improving an average.

Use the same CBIG revision, verified MNI6 1 mm Schaefer 100/400/1000 volumes,
official key/name tables, pinned fsaverage 164k and fsLR 32k registration spheres,
cortex masks and vertex areas as `transform-quality-v1`. Both hemispheres and all
three parcel counts are required. Explicit label hemisphere declarations come
from the verified official lookup tables; generic runtime diagnostics never
infer hemisphere from names or keys.

For each case, record source voxels, supported fsaverage samples and supported
fsLR output counts for every key, including zero, absent keys, opposite-hemisphere
keys and keys absent from the published destination. A key present in another
map must not hide loss. All returned values and missingness must remain identical
to the pre-diagnostics public API. Count opposite-hemisphere output vertices
without silently masking or relabelling them.

Compare the released native barycentric aggregate stage with native largest
contributor selection, Workbench BARYCENTRIC aggregate/largest, and Workbench
ADAP_BARY_AREA aggregate/largest. Sampling coordinates and the first-stage
labels are fixed. Workbench uses the exact pinned registered spheres, cortex
ROIs and declared vertex areas. Target cortex masking applies to every method.
Record Workbench version and method differences; an area-aware external result
does not qualify a new native backend or alter released operator identities.

Acceptance for native largest contributor comparison: zero categorical and
availability differences from Workbench BARYCENTRIC `-largest` on all six cases.
Candidate retention gate: every nonzero source key declared for the hemisphere
must retain at least one supported target vertex, and no opposite-hemisphere
key may occur on supported target cortex, in all six cases. Preserve failures.
Record disagreement and per-parcel Dice against the same-revision published
fsLR atlas as diagnostic pipeline compatibility, not anatomical truth. Do not
replace the no-loss gate with a favourable Dice threshold.

Explicitly report whether any candidate passes the retention gate. If none
does, retain the limitation and document the evidence needed for further
sampling research. Do not force rare keys into unrelated vertices. Independent
manual references and expert review remain separate work.

Primary method documentation:
https://www.humanconnectome.org/software/workbench-command/-label-resample

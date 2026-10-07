# Transform quality: 7 October 2026

The volume transforms have substantially stronger supporting evidence after
this campaign. Native cortical resampling agrees with Workbench on real
parcels. **Fine-parcel volume-to-surface projection remains a material
limitation: it loses three source parcels at 1,000 parcels.** We cannot yet
give an independently established anatomical error bound.

[Visual review packet](evidence-20261007/index.html) |
[Unrated expert review form](evidence-20261007/human-review.csv) |
[Frozen protocol](protocol.md) |
[All volume regions](evidence-20261007/volume-02-regions.csv) |
[All surface parcels](evidence-20261007/surface-02-parcels.csv)

The ten worst regions per volume cell and ten worst parcels per surface case
are also listed in [volume](evidence-20261007/worst-volume-regions.csv) and
[surface](evidence-20261007/worst-surface-parcels.csv) review tables.

## Volume transforms

Both released MNI6/MNI2009c image pullbacks were evaluated on native 1 and
2 mm inputs and target grids, with identity resampling as the control.
These native-2mm-source experiments supplement the original release's
1mm-source experiments; they do not replace or directly reproduce them.

| Source to target | Grid | Brain-mask Dice, identity → warp | T1 correlation, identity → warp | T2 correlation, identity → warp | Regions with worse Dice |
|---|---:|---:|---:|---:|---:|
| MNI6 → MNI2009c | 1 mm | .968 → .979 | .765 → .924 | .703 → .838 | 3 / 69 |
| MNI6 → MNI2009c | 2 mm | .968 → .971 | .810 → .930 | .736 → .849 | 27 / 69 |
| MNI2009c → MNI6 | 1 mm | .968 → .979 | .745 → .893 | .704 → .826 | 3 / 69 |
| MNI2009c → MNI6 | 2 mm | .968 → .970 | .798 → .891 | .705 → .827 | 23 / 69 |

- Across all four grids, **4,178,140 in-brain voxel centres** were checked:
  no nonfinite or nonpositive finite-difference Jacobians. Image-pullback
  Jacobians span **0.426–2.277**. The exterior 3 mm shells also have no sampled
  nonpositive/nonfinite Jacobians. This is a sampled check, not a proof of
  continuous invertibility between samples.
- Dense in-brain inverse consistency has maximum **0.174 mm**; each cell's
  95th percentile is below **0.019 mm**. Round-trip error measures numerical
  consistency, not distance to a true anatomical correspondence.
- Actual public R API T1 and HOCPA outputs match independent SimpleITK
  sampling **exactly at every voxel**, in both directions at 2 mm, with equal
  geometry. This ties the diagnostic H5 sampling to the released consumer.
- Of the 69 regions, 48 are cortical HOCPA and 21 are HOSPA structures
  (including cerebral tissue classes). At 1 mm the worst regional Dice is
  **0.838, Angular Gyrus**, with 95th-percentile boundary distance **2.45 mm**.
  These labels are dependent references, not manually paired ground truth.
- T2 correlation improves in **274/276 regional cells**. The two exceptions
  are the right accumbens in the MNI2009c → MNI6 direction. T2 is an additional
  contrast; its upstream construction is not independent of T1 registration.

### Adverse results and reference controls

The original unfilled brain-mask boundary metric does not improve uniformly:
MNI6 → MNI2009c HD95 worsens from **5.10 to 6.40 mm** at 1 mm and **4.47 to
5.66 mm** at 2 mm. At 2 mm its mean boundary distance is essentially unchanged
(2.6693896 → 2.6694003 mm). These results remain in the primary receipt.

A post hoc control found 10,714 enclosed hole voxels in the MNI6 1 mm mask,
versus none in the MNI2009c mask. With enclosed holes filled in both masks,
exterior-boundary mean distance improves in every cell: about **1.26 →
0.80/0.82 mm** at 1 mm and **1.38 → 1.24/1.25 mm** at 2 mm. This explains why
whole-mask boundary distance mixes differing mask definitions with alignment;
it does not erase the adverse primary result. See
[mask control](evidence-20261007/masks-01-receipt.json).

Posterior superior temporal cortex is the worst 2 mm label, with Dice
**0.568–0.574** and HD95 **4.90–5.66 mm**. Within MNI6 alone, its native 2 mm
label occupies 1,830 voxels, versus 809 after sampling the 1 mm label onto
that same grid: **Dice 0.613 without any inter-template transform**. This
shows a substantial upstream resolution inconsistency. A post hoc control
using both templates' 1 mm labels, sampled directly to the 2 mm target,
raises this region's Dice to **0.819/0.882**. The original cases are retained.
See [resolution control](evidence-20261007/resolution-03-receipt.json).

## Cortical surface resampling

Full meshes, both directions and hemispheres, actual cortex masks, and
Schaefer 100/400/1000 parcels were tested through the public API. Workbench
inputs are checked against the operator's coordinate, topology, mask and
area hashes, including the fsLR sphere registered into fsaverage space.

- **12/12 label cases: zero disagreements across 1,067,418 jointly supported
  target label values** against Workbench **1.5.0 BARYCENTRIC** aggregate
  voting. Availability also agrees exactly.
- Four real sulcal-depth/curvature maps have maximum absolute difference
  **3.88 × 10⁻⁶**, below the unchanged 5 × 10⁻⁵ numerical threshold.
- This does **not** overturn the historical 3/4 failures on the frozen
  analytic-field Workbench experiment: inputs and Workbench versions differ.
  The historical S4 diagnostic JSON actually contains 18 unique case-specific
  target queries (1/6/4/7), rather than the 17 described in older prose.
- Workbench ADAP_BARY_AREA changes **0.029–2.479%** of labels relative to
  native ordinary barycentric resampling. It is a method-sensitivity
  comparison, not a parity gate. Workbench recommends that area-aware method
  for categorical data, especially downsampling.

Agreement with the published destination atlas is less exact:

| Parcels | Disagreement with published destination, all four routes | Typical median parcel Dice |
|---:|---:|---:|
| 100 | 7.75–8.47% | .925–.931 |
| 400 | 14.39–15.11% | .859–.867 |
| 1000 | 21.88–22.39% | .787–.800 |

These percentages include unavailable target cortex. Downsampling leaves
351 left / 390 right cortical vertices unavailable; upsampling leaves
1,529 left / 1,217 right unavailable. Both implementations report this
support difference; it is not a Workbench disagreement. Published CBIG fsLR
1000 already lacks keys 533 and 903, so their absence after upsampling is
not evidence that our resampler discarded an available source parcel.

## Population volume-to-surface projection

The source MNI and destination fsLR Schaefer files come from the **same pinned
CBIG revision**, with every source key/name verified against its official
lookup table and destination label table. This avoids silently comparing
different atlas naming/order versions.

| Parcels | Left disagreement | Right disagreement | Median parcel Dice, L / R |
|---:|---:|---:|---:|
| 100 | 12.01% | 14.06% | .890 / .881 |
| 400 | 23.08% | 24.66% | .788 / .779 |
| 1000 | 35.16% | 37.26% | .655 / .637 |

The prospective **no-label-loss criterion fails** for fine-parcel projection.
This is a campaign finding, not a retroactive alteration of release gates.

| Source key | Region | MNI voxels | Intermediate fsaverage vertices | Final fsLR vertices | Published fsLR vertices |
|---:|---|---:|---:|---:|---:|
| 214 | LH DorsAttn Post 42 | 650 | 3 | 0 | 49 |
| 710 | RH DorsAttn Post 26 | 417 | 1 | 0 | 34 |
| 903 | RH Cont Cing 1 | 98 | 67 | 0 | 0 |

All 500 expected hemisphere parcels survive the first projection stage;
these three disappear during the subsequent ordinary surface resampling.
Their already weak or boundary-limited sampling support matters. There are
also **2–3 left-labelled vertices in right-hemisphere projections**, depending
on parcel count. We have not silently masked, reassigned or repaired them.

Use these population projections with explicit regional coverage checks.
For a published atlas already available on the required surface, using that
surface file avoids an additional volume-to-surface conversion. Check its own
label inventory too: the published CBIG fsLR1000 files already lack two keys.
The disagreement maps and all per-parcel values make the tradeoff reviewable.
No claim of subject-specific localization accuracy is supported here.

## Independence and review

| Evidence | What it supports | What it cannot establish |
|---|---|---|
| SimpleITK and Workbench comparisons | Independent implementation agreement | Anatomical correctness |
| T1 alignment and mask boundaries | Utility on registration images | Held-out anatomical error |
| HOCPA/HOSPA paired template labels | Regional diagnostic concordance | Independent truth; target labels were mapped upstream |
| Published Schaefer destinations | Independent processing compatibility | Independent anatomy; labels were projected from fsaverage6 |
| T2 contrast | Additional-contrast agreement | Fully independent anatomy; upstream registration dependence remains |
| Visual packet | Local review and defect discovery | Expert sign-off until actually reviewed |

The available CerebrA manual atlas targets **MNI2009cSym**, not our exact
asymmetric pair, so it was not substituted as a reference. No independently
paired anatomical landmarks have been obtained. The review form remains
blank. AI inspection of representative panels found reduced T1 differences
and larger widespread projection disagreements than surface-resampling
disagreements; it is not human anatomical certification. Volume panels consistently show the left hemisphere on the left;
images being compared share the same target grid.

Follow-up Motes:

- `bd-01M4AZXRPXE47K2EZNN1GYHBVP`: fine-parcel loss, coverage and hemisphere
  diagnostics, including investigation of improved sampling methods.
- `bd-01M4AZXRQTRCPJS1E3447BVFNP`: independent paired references and expert
  anatomical review.
- `bd-01M4B0JAS92PTS7R5BJEHPEY24`: atlas version and resolution consistency.

Primary provenance: [CBIG Schaefer release](https://github.com/ThomasYeoLab/CBIG/blob/634f676630929a71297852d01dd92a287103e861/stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/README.md),
[Workbench label method](https://www.humanconnectome.org/software/workbench-command/-label-resample),
[MNI template construction](https://www.bic.mni.mcgill.ca/ServicesAtlases/ICBM152NLin2009),
[HCP T2 import](https://github.com/templateflow/tpl-MNI152NLin6Asym/commit/40c0c1c4c5cd72d2b6c25237078899b3120de231).
Upstream raw assets remain external; their existing licenses and attribution
apply. See also the release's transform and surface input license manifests.

## Reproduction and attempt history

Source baseline `c1dc2a4e5e63f2b3af384b3107dd24026b9eef14`; R 4.3.3;
neurotransform 0.2.0 at `933edddda462593941e167726e8aaa7168ff103a`;
Workbench 1.5.0; SimpleITK 2.5.3; NiBabel 5.4.2. No runtime package changes
or dependencies were added. Five analytic metric controls pass.

Repository validation: `devtools::test()` reports **2,770 passes, zero failures,
44 console warnings and two opt-in skips** (the saved per-case result contains
43 warnings; both counts are preserved). `devtools::check()` completes with
**zero errors, zero warnings, two notes**: the existing 30 MB installed package
size and inability to verify the machine's current time. See
[check receipt](evidence-20261007/checks.json). The final evidence audit verifies
267 raw/input/packet hashes and the exact script/protocol bindings.

The devbox work directory is
`/work/code/neuroatlas/data-raw/surface-transforms-v1/work/quality-20261007`.
Use the existing pinned R environment and Python environment described in
the surface release handoff. Scripts take explicit work/output paths;
output directories must be fresh. The execution order is:

1. `fetch-inputs.py WORK/inputs`; `prepare-surfaces.py WORK`;
   `fetch-cbig-volumes.py WORK`.
2. Populate `WORK/cache` through the verified public API for both volume
   routes and four surface domains/operators, plus projection assets.
   This campaign copied the already verified surface/projection cache and
   fetched both volume transforms through `get_template_transform()`.
3. `volume-quality.py WORK WORK/volume-02`;
   `Rscript public-api.R WORK WORK/public-03`.
4. `surface-quality.py WORK WORK/public-03 PINNED_SURFACE_INPUTS WORK/surface-02`;
   `t2-quality.py WORK WORK/t2-01`.
5. `diagnose-resolution.py WORK WORK/resolution-03`;
   `mask-boundary-audit.py WORK WORK/masks-01`;
   `Rscript projection-stages.R WORK WORK/stages-01`.
6. `test-metrics.py`; `build-report.py WORK PINNED_SURFACE_INPUTS OUTPUT`.
7. Run `devtools::test()` and `devtools::check(document = FALSE, manual = FALSE)`
   in the pinned environment, saving their results as `WORK/package-tests.rds`
   and `WORK/package-check.rds` and their logs alongside them. Then run
   `Rscript record-checks.R WORK`; `verify-evidence.py WORK OUTPUT` (save output
   as `WORK/verify.log`); `finalize-evidence.py WORK OUTPUT`.

Run from the package root. `build-report.py` also copies `WORK/metric-tests.log`.
Full raw outputs, failed attempts, commands and downloaded assets remain in
WORK. Compact CSVs, receipts, input hashes and review figures are committed.
The original release receipts and thresholds are unchanged.

Attempt history is retained rather than suppressed:

- `public-01` stopped because the QA driver omitted background key zero
  from its projection label table. `public-02` corrected that.
- `surface-01` used the unregistered fsLR sphere and stopped on an unmatched
  TemplateFlow/CBIG label name. Neither its partial comparisons nor those
  projection inputs are accepted evidence. The corrected driver binds all
  sphere/mask/area arrays to the exact public operator hashes.
- `public-03` uses same-revision CBIG MNI/CIFTI labels, verified by the official
  lookup table. `surface-02` completes all cases with registered spheres.
- Resolution, mask-hole and projection-stage controls are explicitly post
  hoc investigations; they do not replace the prospective results.

The volume and resolution drivers were repeated in fresh directories after
standardizing figure orientation and using ASCII-only script source. The
underlying metric values were checked against the earlier attempts.

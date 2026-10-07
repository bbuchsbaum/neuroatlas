# Native ordinary surface integration

## Current release slice

Version 0.2.0 admits the four exact, pinned fsaverage 164k / fsLR 32k domain
pairs through `get_template_transform()` and `apply_template_transform()`.
`get_surface_geometry()` fetches original provider assets under their byte locks.
Broad surface names remain planned. Admission requires the pinned engine revision
and exact ordered geometry, hemisphere, masks, areas and method identity.
Equal domains additionally require a proven diagonal operator with unit weights;
an arbitrary cached operator cannot inherit identity qualification.

Population cortical projection and caller-aligned ribbon sampling have a separate
[protocol](projection-v1/README.md). The devbox refresh, current source bindings,
engineering checks and review boundaries are indexed in
[the release evidence](qualification/devbox-20261006/final/index.md).
Inputs are downloaded from their original providers; this slice does not bundle
the pinned input set. Review [the per-asset license audit](LICENSES.md).

The September report below is historical. Its source hashes, engineering counts,
then-current admission status and dependency environment describe that snapshot.
Its failed strict Workbench comparisons and numerical thresholds remain retained.
For fresh builds without a sibling checkout, set `NEUROATLAS_ENGINE_BINDING` to
the receipt produced by `setup-engine.R`; see
[the machine handoff](../../plans/resume-neuroatlas.md).

## Historical qualification: 2026-09-27

The explicit-domain surface API is implemented and the numerical gates in
`native-contract-v1.json` pass for the four pinned fsaverage 164k / registered
fsLR 32k routes. This qualifies native closest-point barycentric interpolation
and the tested data policies on those inputs. It does not establish Workbench
parity, registration accuracy, area conservation, or reversibility.

## Public consumer

- `surface_domain()` records exact ordered geometry, hemisphere, registration,
  revision, mask and optional area identity.
- `surface_geometry()` verifies arrays or GIFTI geometry against that identity.
- `surface_data()` binds continuous, probability or categorical values to the
  caller-declared exact domain. GIFTI hemisphere and intent are checked; a file
  alone cannot establish its vertex order. Label tables and valid key zero survive.
- `get_surface_transform()` builds or reloads a directed native operator. Its
  cache binds domains, masks, method and installed engine. Ownership receipts,
  hashes, offline behavior, symbolic-link rejection and selective cleanup are tested.
- `apply_surface_transform()` returns values, coverage, availability/status and
  provenance. Source exclusion precedes row normalization. Target masking follows
  it. Missingness policies are explicit. Label keys are voted, never averaged.
  Probability channels retain partial mass; only output roundoff within 1e-12 of
  [0,1] is clipped. Inputs remain strictly bounded.

DESCRIPTION pins neurotransform 0.2.0 at
`933edddda462593941e167726e8aaa7168ff103a`. Validation used an isolated rebuilt
library, not a global replacement. The installed build has no RemoteSha field;
source-file and installed-artifact hashes bind it to the published revision.

Caller-supplied operators still report `qualification = "unqualified"`.
The generic named-space registry remains advisory for surfaces: its broad names
cannot encode these exact domains and method-specific evidence. No surface
alias has been promoted to a generally qualified automatic route.

## Frozen numerical acceptance

Contract SHA-256:
`e9f217406649773f795a00a75cf0f5172986b6d3724408e4cc55681a2fed882d`.
The 1e-12 absolute weight/value and relative point tolerances were recorded before
fresh fixtures were evaluated. No tolerance was widened after observing results.

The independent Python oracle considers every source triangle, using SVD
least-squares interior projections and all three closed segments. It does not
use the production closest-point implementation, spatial index or pruning.
Each real route uses 256 uniform targets and the 64 smallest positive-weight
targets; deduplication leaves 319 or 320 distinct targets. Fourteen rotated
six-vertex synthetic fixtures add 84 queries, including vertices, edges, interiors
and signed boundary perturbations through 1e-8.

| Route | Exhaustive queries | Max weight error | Masked value error | Label / availability mismatches |
|---|---:|---:|---:|---:|
| L 164k to 32k | 319 | 3.41e-14 | 6.94e-16 | 0 / 0 |
| L 32k to 164k | 320 | 1.10e-14 | 7.77e-16 | 0 / 0 |
| R 164k to 32k | 319 | 2.41e-14 | 5.55e-16 | 0 / 0 |
| R 32k to 164k | 320 | 1.10e-14 | 5.55e-16 | 0 / 0 |

Full-density gates cover positive finite weights, unit row mass, triangle support,
constants, convex bounds and unchanged outputs after reversing face order and
winding. Independent accumulation checks all 392,668 target rows under pinned
masks, with maximum difference 3.33e-16. All-source constant/bounded checks also
pass. Synthetic tests cover both label policies, exact ties, all-excluded support,
all missingness policies and a 1e-8 contributor retained as the sole included
source. Exact coincident-vertex support is asserted. Real probability tests cover
one, zero, partial mass and missing values under propagation and omission.

Each case manifest binds sample indices and every consumed geometry/policy file.
The consumer receipt binds its source snapshot, engine evidence and case manifests.
Tampering checks demonstrate rejection of changed sample selection or policy bytes.
The final evidence index also binds execution receipts and the supplemental policy
receipt. Full arrays and logs are retained in `work/integration-20260927/native-03`;
small durable receipts are copied into `qualification/native-ordinary-v1`.

## Coverage and visual inspection

| Route | Available / target vertices | Area-weighted x-ramp RMSE | Max x-ramp error |
|---|---:|---:|---:|
| L 164k to 32k | 29,345 / 32,492 | 9.81e-5 | 0.00852 |
| L 32k to 164k | 148,426 / 163,842 | 3.09e-4 | 0.02236 |
| R 164k to 32k | 29,326 / 32,492 | 8.27e-5 | 0.00631 |
| R 32k to 164k | 148,709 / 163,842 | 2.88e-4 | 0.01947 |

These ramp errors measure approximation against an analytic field evaluated at
target vertices; they are descriptive, not oracle discrepancies or area-conservation
tests. The larger errors concentrate around medial-wall boundaries where source
exclusion changes interpolation. Longitude/latitude plots for all four routes
show coherent categorical boundaries, the expected hemisphere-specific medial
walls, and explicit unavailable strips (black) adjacent to masked targets (gray).
Visual review checks gross boundary/hemisphere failures; it is not anatomical
registration validation. Plots have independently scaled signed-error colors.

## Preserved limitations

The original Workbench ordinary full-field threshold remains 5e-5. Three of four
comparisons failed; their 17 near-edge disagreements and upstream independent
geometric diagnoses remain retained as retrospective evidence. A small positive
third contributor can yield an availability or categorical difference after
masking. These results therefore do not support a universal Workbench-compatible
metric or label claim. Adaptive area resampling remains experimental upstream
and is not exposed by this consumer.

Population MNI6/MNI2009c registration fusion, aligned white/pial ribbon sampling,
and directed surface-to-volume rasterization are separate subsequent work.
Neither interpolation nor backprojection is an inverse of discarded information.

## Reproduction

From the repository root, install the pinned engine into an isolated library and
set `R_LIBS` to it. The input lock and earlier preparation scripts provide the
exact assets. The verification scripts currently also require the sibling
neurotransform checkout and its retained source/install binding receipts.
Use a fresh output directory on every attempt:

```sh
Rscript data-raw/surface-transforms-v1/qualify-native.R /tmp/native-new-attempt
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 \
  data-raw/surface-transforms-v1/qualify-native-oracle.py /tmp/native-new-attempt
Rscript data-raw/surface-transforms-v1/qualify-native-policies.R /tmp/native-new-attempt
python3 data-raw/surface-transforms-v1/test-native-evidence.py /tmp/native-new-attempt
```

`RGL_USE_NULL=true` avoids the existing macOS rgl/X11 context abort in the broader
package tests; it does not alter surface interpolation. Use a valid process locale
such as `LC_ALL=en_US.UTF-8` for R CMD check. Network/optional-dependency test skips
and existing warnings must remain visible. Manual and vignette generation are
not covered by the present package check.

## Final package verification

- Focused consumer/domain/cache/planner/volume checks: 246 assertions, zero
  failures, test warnings or skips (`consumer-final.log`).
- Full development suite before the small probability correction: 1,603 passes,
  41 warnings, 89 skips. All 41 warning signatures match the unchanged baseline.
- Final installed-package suite in R CMD check: 1,458 passes, zero failures,
  41 warnings, 95 skips. Differences reflect the installed/check environment and
  source-only/CRAN guards; skipped gates are not counted as passes.
- Final `--as-cran --no-manual --ignore-vignettes` check: **0 errors, 2 warnings,
  0 notes**. Examples, including `--run-donttest`, pass. Warnings concern existing
  CRAN submission metadata/dependencies/403 URLs, the omitted prebuilt vignette
  index, and undeclared `png` use in existing CPU plot tests. Optional suggested
  packages were unavailable; `_R_CHECK_FORCE_SUGGESTS_=false` was explicit.
- Scientific review closed the probability, manifest binding, engine binding
  and structural-zero findings. `git diff --check` passes.

The R CMD build was staged from current source, excluding only paths already
listed in `.Rbuildignore`; this avoids copying broken annex links in the ignored
preparation cache. A per-file staging manifest shows no source drift. The default
R library, volume V1 artifacts, upstream repositories and remote clusters were
not changed. No commit or push was performed.

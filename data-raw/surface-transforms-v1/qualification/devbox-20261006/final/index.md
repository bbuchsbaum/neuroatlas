# Cortical transform release evidence

Validated 2026-10-06 on the Google Cloud devbox for neuroatlas 0.2.0.
Publication status is recorded separately in Mote and the GitHub release.
The source-bound consumer receipts qualify the final R sources, DESCRIPTION,
NAMESPACE, registry and input manifests. Raw inputs, arrays, operator caches,
libraries and QA figures remain local; this directory contains small receipts
and logs. Engine binaries are machine-specific and must be rebuilt and bound on
another machine.

## Supported scope

| Method | Inputs and direction | Qualification |
|---|---|---|
| Native closest-point barycentric | Exact fsaverage 164k to fsLR 32k and reverse, L/R separately | Four pinned directed routes |
| Exact index identity | Equal verified domains, diagonal unit weights, pinned engine | Structural proof; normal masking and missingness policies apply |
| CBIG population registration fusion | Exact MNI6/MNI2009c to pinned fsaverage 164k or fsLR 32k, L/R | Four locked 1 mm / 2 mm source grids; scalar, label and probability sampling |
| Aligned ribbon sampling | Caller-declared aligned white/pial RAS nodes and source frame | Synthetic oblique-grid policies at 3, 5 and 9 nodes; equal node weights |

Broad surface aliases, other densities, CIFTI, directed surface-to-volume
mapping and specialist templates remain future work. Ribbon sampling does not
fit subject registration and differs from Workbench's voxel-intersection method.
Other grids in a declared population frame can be sampled, but have no retained
grid-specific qualification.

## Numerical checks

[Native consumer](native/consumer-receipt.json),
[independent exhaustive oracle](native/oracle-receipt.json) and
[full-density policies](native/policy-receipt.json) pass the unchanged native
contract. There are 1,362 exhaustive queries and 392,668 target policy rows.
Maximum weight discrepancy is `3.397282455352979e-14`; sampled categorical and
availability mismatches are zero. Altered sample selection and policy inputs are
rejected by the evidence tests.

[Projection consumer](projection/consumer-receipt.json),
[independent SciPy oracle](projection/oracle-receipt.json) and
[external references](projection/reference-receipt.json) pass the frozen
projection contract. The oracle checks 108 cases and 6,365,248 vertex-map values,
with maximum scalar discrepancy `2.842170943040401e-14`, zero label mismatches
and zero availability mismatches. Maximum finite-mass discrepancy is
`1.3322676295501878e-15`. SimpleITK composed coordinates agree exactly; Workbench
point sampling has maximum scalar discrepancy `8.72968317078282e-08` against the
predeclared `5e-5` float32 threshold. Five evidence mutations are rejected.

CBIG target ordering has identical ordered triangles and zero nearest-index
mismatches. Its sphere coordinates differ from the pinned TemplateFlow spheres;
the reference receipt reports those differences. The original CBIG FSL image
and MNI6 reference have identical sampled cortical support but 38,359 differing
whole-volume voxels. This audit does not establish whole-volume equivalence.

The historical strict ordinary Workbench comparison still fails in three of
four cases, with 17 near-edge disagreements. Its `5e-5` ordinary and `2e-6`
near-edge thresholds remain unchanged. Passing the native contract does not
establish Workbench parity, anatomical accuracy, reversibility or conservation.

## Actual data and visual review

[Public examples](examples/public-examples-receipt.json) exercise both templates'
2 mm T1w maps on all four cortical domains, with identical offline replay.
Harvard-Oxford cortical labels in their original MNI6 frame retain all 48 region
keys plus background zero on both hemispheres and densities, with source atlas
identity retained. Available vertices are:

| Domain | Left | Right |
|---|---:|---:|
| fsaverage 164k | 149,955 | 149,926 |
| fsLR 32k | 29,345 | 29,326 |

[Per-label areas](examples/label-areas.csv) and
[area changes](examples/label-area-changes.json) are descriptive. Nonbackground
fsLR versus fsaverage area changes range from -18.2641% to -0.4280% on the left,
and -14.6916% to +2.5235% on the right. No area-conservation claim is made.

[AI visual review](examples/visual-qa-review.json) records five hashed figures:
both hemispheres on midthickness and inflated surfaces, medial-wall exclusion,
and sampling points over exact source images. All
[20 binary plotting inputs](examples/plot-input-equivalence.json) match the
reviewed outputs exactly. Human review was not performed. These dependent
population overlays are sanity checks, not held-out anatomical references.
Figures stay local because their upstream redistribution terms are separate.

## Engineering checks

The full development suite passes **2,770 assertions, zero failures, 44 warnings
and two opt-in skips**. The [full package check](full-check-summary.json) passes
with **zero errors, zero warnings and three notes**; installed tests, examples,
vignettes and the PDF manual pass. Notes concern installed size (30 MB), inability
to verify current time, and the missing optional HTML `tidy` validator.

One cache-initialization example exceeded five seconds and was marked
`donttest`. All executable R expressions remain identical to the full-check
snapshot, as recorded in
[the source comparison](executable-source-equivalence.json). The
[documentation follow-up](release-docs-check-summary.json) rechecks examples,
vignettes and the PDF manual with zero errors and warnings; its tests are
intentionally skipped because the unchanged executable code already passed the
full development and installed suites.

The project lint gate has zero findings. Workflow syntax validation and seven
offline release-guard tests pass. The pkgdown website builds successfully.
Complexity remains advisory under the owner's selected policy; see
[the separate review](../../../../../plans/complexity-review.md).
Hosted checks and release publication are recorded in Mote after pushing.

## Reproduce and boundaries

Follow [the machine handoff](../../../../../plans/resume-neuroatlas.md),
[native qualification](../../../native-qualification.md) and
[projection protocol](../../../projection-v1/README.md). Rebuild the pinned
engine and obtain your own installed-artifact receipt; never borrow a devbox DLL
hash. Fetch checksum-locked inputs from their original providers and review
[the license audit](../../../LICENSES.md). Serialize jobs sharing a cache.

The baseline, earlier public workflow, failed JSON driver attempt and cache
exclusion timeout remain retained. `pre-slow-example-source/` contains the
previous passing byte-bound receipts before the documentation-only slow-example
annotation. These snapshots do not replace the final consumer source bindings.

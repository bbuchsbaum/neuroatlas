# Surface engine admission failure

## Update: original defect resolved upstream

Verified published HEAD `933edddda462593941e167726e8aaa7168ff103a` on
2026-09-26. Its implementation is identical to
`9ab40ffd9b3b9fd46ac2e805826cbc8fde60b429`; the final commit adds evidence.
All 51 source, installed-artifact and evidence hashes in the upstream binding
matched. The retained rebuilt 0.2.0 library passed this project's unchanged
compiled probe and all 154 focused domain, route and volume-transform assertions.
The historical failure below is preserved; it is no longer the current blocker.

The upstream `output/surface-completion/qualification.md` records indexed
geometry shared with the sampler, mesh admission, masks/missingness, categorical
policies, adjoint/reverse semantics, and experimental adaptive area support.
Full-target qualification still fails the original Workbench metric tolerance
in three of four ordinary cases and adaptive upsampling cases. The ordinary
discrepancies affect 17 near-edge targets; independent geometry agrees with
native weights within 1.58e-14. This is a documented comparator disagreement,
not the original opposite-face bug and not a strict parity pass.

The production pin and route statuses remain unchanged pending the declared
near-edge agreement policy and remaining route qualification. See `work-log.md`
for the fresh consumer receipts. No upstream implementation was changed here.

## Historical reproduction

2026-09-26. Surface execution is blocked on the installed neurotransform
barycentric engine. This is independent of remote access or Slurm readiness.

Run the retained analytic probe (the output path must not already exist):

```sh
Rscript data-raw/surface-transforms-v1/check-surface-engine.R receipt.json
```

The closed octahedral sphere has vertices at the six signed coordinate axes.
The query is `(1,1,1)/sqrt(3)` and vertex values are the z coordinates.
The positive-octant face has equal barycentric weights, giving `1/3`.
Reordering triangle rows cannot change the geometry or that answer.

The installed compiled engine returns `1/3` with the original face order and
`-1/3` after reversing the rows. In the second case it uses vertices on the
opposite side of the sphere. The `1e-12` absolute tolerance was set before the
probe; the discrepancy is not floating-point noise. The admission script exits
1 and writes its results, contributing vertices, weights, session information,
script hash, and compiled-library hash.

Retained receipt: `work/surface-engine-probe-01.json`. Engine version: `0.1.0`.
The installed package has no RemoteSha, so its source revision is not asserted.
Compiled library SHA-256:
`d5eaf778e172114be65e88513eeea971d3f07bb127aeddb22c30c46a988db4c4`.

The clean sibling source checkout is at the revision pinned by neuroatlas:
`9e7550d45d6d6b06155b27717057f4137aaf666e`. In
`neurotransform/src/rcpp_bary.cpp`, `bary_point()` ranks accepted orthogonal
triangle projections by `abs(u)+abs(v)+abs(w)`. For nonnegative barycentric
weights that sum is one, irrespective of spatial distance. It neither selects
the containing spherical face nor measures a point-to-triangle distance.
It also scans every source face for every query. This source explains the
observed failure; the receipt independently proves installed behavior.

The current surface engine implements ordinary barycentric or nearest-vertex
resampling, not Workbench ADAP_BARY_AREA. The successful Nibi smoke used
ADAP_BARY_AREA and establishes oracle readiness only. A future comparison must
match methods explicitly, including masks, missing support, labels, and areas.

Next gate: repair and test face selection in neurotransform, including the
analytic case, face-order permutations, opposite-side exclusion, and boundary
cases. Then pin the repaired engine and compare its declared algorithm against
Workbench at both template densities and hemispheres. Do not substitute a
nearest-neighbor fallback or relax the invariant to activate the route.

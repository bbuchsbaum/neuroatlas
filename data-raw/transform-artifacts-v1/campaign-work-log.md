# Nibi campaign work log

This log preserves campaign mechanics separately from transform qualification.
Do not include credentials, account names, private paths, or scientific data.

## 2026-09-19 — local preparation

- Frozen the V1 MNI152NLin6Asym to MNI152NLin2009cAsym inputs in `routes.json`:
  1 mm brain T1w/masks plus matched HOCPA label maps, including exact byte
  counts, SHA-256 values, and source/target header geometry.
- Added a fail-closed qualification policy. Numeric thresholds remain unset;
  `qualify.R` and `assemble-release.R` refuse to proceed until a benchmark
  review explicitly approves them.
- Added required `neuroim2` visual QA for native context, candidate
  checkerboard, mask/label edges, and Jacobian displacement context. It is
  checksummed qualitative review evidence, not a numerical substitute.
- Added Nibi campaign definitions with separate production and smoke roots.
  They initially remained deliberately non-submittable until the authenticated
  Nibi discovery recorded below; checked-in templates still retain literal
  root sentinels.
- The installed `niflowr` 0.1.0 package is older than the checked source API:
  it does not export `ni_ants_register_to_template()`. Development deployment
  may follow the upstream GitHub reference once the required exports have been
  checked; every smoke receipt must record the installed immutable `RemoteSha`.
  A moving development reference is not release evidence.
- Filed remoteslurm issue #2: an unconfigured `user@host` connection could
  print that its SSH master was established even though it could not be reused.
  Local remoteslurm commit `c70e37e` now requires an effective `ControlPath`
  and verifies reuse before that success message; its focused connection and
  SSH-option tests pass. It remained open until the named Nibi profile
  completed the live post-MFA liveness check recorded below.
- Added a named local Nibi profile with MFA and an explicit reusable
  `ControlPath`. `remoteslurm --json doctor nibi` confirms the profile and
  socket configuration; its only failing check is the expected absent master
  before the user completes MFA.

## 2026-09-19 — authenticated Nibi discovery

- Completed Duo MFA through the named Nibi profile. The reusable SSH master,
  remoteslurm stub, Python 3.11, and Slurm discovery all succeeded.
- Re-ran the original bare-host reproducer against remoteslurm `c70e37e`: it
  now stops before authentication with the required `ControlPath` diagnostic
  and does not claim a master was established. Closed remoteslurm issue #2
  with this evidence and the focused regression-test result.
- Observed an eligible CPU account association and CPU partition policy
  locally; the Nibi profile holds those user-specific settings. The selected
  partition permits the five-hour build allocation while the smoke is bounded
  to 30 minutes, one CPU, and 4 GiB. Build is bounded to eight CPUs/32 GiB and
  qualification to four CPUs/16 GiB.
- Verified that `apptainer/1.4.5` loads in a non-login shell and added it to
  both campaign preambles, with its cache placed under `SCRATCH`. The campaign
  definitions compile against the current remoteslurm source.
- Interactive Nibi discovery found RNifti, neuroim2, and jsonlite, but the
  batch module environment did not preserve those user-library paths. Git and
  rsync are available.
- No campaign run or Slurm job has been created.

## 2026-09-20 — first smoke attempt

- The one-unit smoke campaign passed its remoteslurm static, remote, and
  fixture preflight and was accepted by Slurm. It exited before ANTs ran
  because the batch module environment did not expose `RNifti` (or
  `neuroim2`) through the isolated project library. No declared output exists,
  and no retry has been authorized automatically.
- The repair makes all direct smoke/validation and later visual-QA package
  dependencies explicit in the isolated library and requires their receipts
  in smoke, build, and qualification provenance. A batch-style dependency
  check must pass before an explicit retry is authorized.

## 2026-09-20 — development runtime and staging

- The user authorized development installs from the current GitHub references.
  An isolated project R library now contains `niflowr`, `neurotransform`, and
  `neuroim2` from their observed upstream commits, plus CRAN `RNifti`, `hdf5r`,
  and the direct compiled imports required by neuroim2. A batch-style check
  loads every package. The smoke script records package version and
  `RemoteSha`; this tracking policy remains ineligible for a release candidate
  without immutable commit evidence.
- The Nibi Apptainer profile was pinned into a lockfile and passed strict
  lock validation, including the locally cached SIF digest.
- Source scripts and the six frozen inputs were synchronized with deletion
  disabled. The remote source marker records the local repository was dirty;
  this is development staging, not a source-release assertion.
- Materialized the ignored sibling smoke campaign from the checked-in sentinel
  template. Its corrected staged scripts resolve their artifact root from the
  campaign workspace rather than from a temporary submission script path;
  sibling placement preserves campaign-relative script and validator paths.

## 2026-09-20 — validated synthetic smoke

- The explicit source-bound child run completed the synthetic registration
  with niflowr's `testing` preset. Both directional H5 files and the warped
  image passed declared cardinality, size, checksum, and stability contracts.
- Its first independent validation receipt correctly failed because campaign
  validators do not inherit a stage preamble. A third linked run adopted the
  completed outputs under a definition that explicitly supplies the isolated
  `R_LIBS_USER`; it independently opened both H5 files with neurotransform and
  passed. That validated child run is closed. The failed execution and failed
  validator receipt remain in the parent lineage.
- The successful non-production provenance records the R version, package
  versions and development SHAs, source/common/config/lock SHA-256 receipts,
  transform byte counts, and output SHA-256 values.
- Filed remoteslurm issue #4: `output` cannot resolve stdout for a
  campaign-submitted array job even when `status` reports the path. The
  mistakenly opened issue #3 was corrected and closed after identifying an
  omitted named-host selector; it was not a product defect.
- This is synthetic non-production evidence only. No production registration,
  visual qualification, release asset, or neuroatlas runtime registry change
  has been made.

## Historical next actions after the synthetic smoke

1. Keep tracking development package commits in each new receipt; before a
   release candidate, replace moving references with immutable commits and
   freeze the corresponding package/container/runtime evidence.
2. Calibrate and approve the numeric qualification policy against the frozen
   identity baseline and official TemplateFlow H5, then run the production
   build and visual-QA qualification campaign. The current policy remains
   deliberately fail-closed.
3. Assemble and publish a `transform-artifacts-v1` GitHub Release only after
   the numerical and human visual gates pass; only then expose the route in
   neuroatlas' runtime registry.


## 2026-09-24 - fitted builds and prospective qualification

- The user reconnected Nibi MFA and authorized pursuing the feature to completion.
  All remote operations use the named Nibi profile and non-deleting sync.
- Release software is pinned to niflowr `3fe349389c659595c4ed21af7d48641486604c5d`,
  neurotransform `9e7550d45d6d6b06155b27717057f4137aaf666e`, and neuroim2
  `e3f0d64b91a5a92962acd653369d5f9a7df885ea`. The ANTs container digest remains
  pinned in routes.json and the isolated Nibi lockfile.
- Build campaign `neuroatlas-transforms-build-v1`, run `production-20260924-02`,
  job `22604958`, completed both candidate and repeat. Definition:
  `dc739313c423e8a760342e425584b05a51f8287f7a58a5fa371e83940777a151`.
  Candidate verification receipt:
  `2e92d025a8b36dbf3cf471859bf774793eee4f442ffb1dddc66f6416a920c54e`;
  repeat verification receipt:
  `af5517c32d58a3de1e830b12f5d92afa7706be4d2e749bf701692f87d11d5e3e`.
  These attest output integrity, not scientific qualification.
- Parent build run `production-20260924-01` retained successful native outputs
  but failed wrapper serialization of an `ni_result` S3 object. The child uses
  the native JSON provenance already written by niflowr and binds its receipt.
- Calibration run `calibration-20260924-02`, job `22604960`, completed. The
  official pair has 6.37-7.27 mm sampled inverse-consistency maxima, and the
  inverse route worsens MI, CC and mask Dice against identity on both grids.
  ANTs and neurotransform nevertheless agree within 0.000443 mm at sampled
  points and essentially machine precision on continuous interior samples.
  The upstream pair therefore remains unavailable. The raw calibration and
  failed provider verdict are retained under the ignored work directory.
- The calibration's false geometry flags were an evaluator defect: numeric
  affine matrices and dimensions match, but NIfTI intent-code attributes differ.
  The corrected check compares numeric geometry exactly, with regression tests.
  This does not explain or erase the official pair's scientific failures.
- Before inspecting candidate quality, independent review approved prospective
  engineering budgets and strict improvement over identity. The plan's pilot
  interpretation is explicitly amended in the policy: synthetic smoke establishes
  exercised mechanics, not anatomical accuracy. Official comparisons and HOCPA
  concordance are diagnostic. No thresholds were selected from candidate results.
- Policy SHA-256:
  `b53ccca3dd5db77016150cdcc9458385f390821e85641918df0d9976dbdc912b`.
  Four frozen coordinate files contain 320 unique points each: 64 in each of
  central, cortical, subcortical, boundary and near-mask-edge strata, with 32 per
  hemisphere. Independent review verified geometry, mask membership and hashes;
  a second generation reproduced all CSV bytes exactly.
- Qualification run `qualification-20260924-01`, job `22606158`, evaluates all
  four direction/grid cells for both fitted builds. Preflight receipt:
  `e5598291415b1e7c52739b79ac8b7fce36679bb4e3645edcd38d615c4bc63f84`.
  The run writes numerical evidence and visual panels. Review-dependent release
  eligibility remains false until reviews are attached to the exact evidence.
- remoteslurm status refresh of running arrays hit `KeyError: '22604489'` in
  `_sample_telemetry` because sstat's parent ID was absent from the task map.
  Waiting via `remoteslurm wait` avoids that defect; terminal verification works.

Current next action: inspect qualification results, preserve every failure,
review the exact visual evidence and distribution notices, then assemble a
release only if all gates pass. Nonlinear registry rows remain planned.

## 2026-09-24 - first fitted-pair qualification fails repeatability

Run `qualification-20260924-01` completed all eight cells. Output-integrity receipt
`8d0a7a90c38c9e2d17d68a14226fe7464e38d4b71f2a1839f6664bfebc03d63f`
is not a scientific pass. All candidate and repeat cells improved MI, CC and mask
Dice over identity, had finite positive Jacobians and exact target geometry,
preserved datatype semantics, and passed independent-engine checks. The largest
inverse-consistency error across both builds was 0.071785 mm. However, repeat
point maxima were 0.233108, 0.169405, 0.258565 and 0.322297 mm, exceeding the frozen
0.1 mm limit in every cell. The unchanged qa.json retains those failures.

The native build command revealed a concrete control gap: host-only
`ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS` was absent from the clean Apptainer
container. Build provenance described eight requested threads but did not prove
that the container received that setting. The rebuild passes ITK threads and
ANTS_RANDOM_SEED through explicit container environment arguments, validates the
execution plan before computing, and uses a single thread with seed 1. This
follows ANTs' reproducibility guidance:
<https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues>.
Qualification likewise passes its four-thread setting through niflowr config.
The qualification policy and landmark files are unchanged.

The planned child build is `production-20260924-03`, with two independent
single-CPU array tasks and a five-hour per-task limit. Previous artifacts and
failed evidence remain intact. Also observed: remoteslurm `get --rsync` omits the
configured ControlPath and fails MFA authentication. Normal small-file get works;
explicit rsync using the observed Nibi ControlPath safely retrieves large files.

### Corrected build allocation

`production-20260924-03` was rejected before execution: Slurm reported that
the 1 CPU / 32 GiB request could not fit the configured partition. The
partition was confirmed UP; previous completed builds peaked at 3.02 GiB RSS.
The child `production-20260924-04` requests 2 CPUs / 8 GiB, retaining one
explicit ITK thread and the unchanged frozen qualification policy. The
rejected attempt and its original campaign definition are retained.

Accepted as job `22607432`; preflight `fcabba0d7a47e8ca3ccf4e65fae489068889f9e9861917eb1be5343046c9cf0e`;
definition `2697fd33eb8709c60e6ce2ca69a00d666e6fb301997aaa0027a6884f072e4817`;
attempt `bb3b30be322d4713a70de30db4029d89`.

Before qualification02, added diagnostic mask and label image round trips
(back to each original 1 mm source grid) required by plan section 7.5.
This does not change primary metrics, invariants, or the frozen policy.
Synthetic tests cover loss, fractional/new labels, nonfinite masks/labels,
and mismatched geometry (28 measurement assertions passed).
Qualification02 preflight passed:
`4d39de48d0aed774f5fc4bbc8f59cd28042431821afadad30a343b013326ccee`.
The source suite passed 1,418 assertions with 41 baseline warnings and 88 skips.

Independent follow-up review identified and resolved three evidence issues
before qualification02: fractional returned labels now have unavailable loss/
Dice diagnostics; native comparison files and the permitted tie/edge mask are
hash-bound to each cell; public integration binds inputs/candidates/references
to reviewed QA and that QA to the public release manifest. It rejects every
label disagreement outside the frozen locations, rather than using a count
budget. Focused checks passed: 8 live-evidence, 30 measurement and 26 assembly
assertions. The independent reviewer reported no remaining blockers in this
bounded follow-up. Policy bytes remain unchanged.

Package check05 passed 0 errors, 0 warnings and 0 notes. An expanded network
check04 exposed a pre-existing matrix-versus-data-frame color-map assertion;
comparison now preserves dimensions/column names and all values, and that real
cached-atlas test passed 23 assertions. The runtime also now diagnoses missing
optional hdf5r support explicitly (42 focused runtime assertions passed).

Final qualification02 preflight after the reviewed evidence fixes: `0b3d3faa2171f9305ecd31d227612b79966da6bf3b604d1ef9f72e013549b43c`.

The root checkout source suite passed 1,429 assertions (41 baseline warnings,
88 skips); check06 had 0 errors, 0 warnings and an environment clock NOTE.
A focused delivery branch based on master excludes the unrelated surface
rendering commits. Its source suite passed 1,388 assertions after correcting
two pre-existing empty-label synthetic surface fixtures for the installed
neurosurf validity contract. Its package check had 0 errors and 0 warnings;
the worktree's .git pointer file is now explicitly excluded from package builds.

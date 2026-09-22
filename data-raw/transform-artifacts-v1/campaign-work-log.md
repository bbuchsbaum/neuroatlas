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

## Next observed action

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

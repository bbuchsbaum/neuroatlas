# Surface preparation work log

## 2026-09-26 - input bundle staged; Slurm preflight blocked

Scope: prepare reproducible inputs and the independent Workbench environment
for fsaverage 164k <-> fsLR 32k, both hemispheres. No public API, registry,
published artifact, or V1 volume-release change.

Completed:

- Downloaded and pinned 28 upstream files, 38,166,858 bytes, in
  `inputs.lock.json`. TemplateFlow files matched their revision-bound annex
  sizes and MD5s before SHA-256 locking. The neuromaps archive matched its
  published MD5 and is additionally SHA-256 pinned.
- Verified four domains, including exact ordered faces shared by anatomical
  and registration meshes, expected vertex counts, positive areas, binary
  masks, and exact equality of the neuromaps/TemplateFlow fsaverage sphere
  arrays. Included vertices: fsaverage L 149,955; R 149,926; fsLR L 29,696;
  R 29,716. These are input checks, not anatomical qualification.
- Created 12 synthetic GIFTIs and reopened each using R/gifti. A separate
  standard-library Python reader also read all 12 fixtures and the four binary
  upstream masks successfully.
- Five preparation regression tests passed, covering missing/corrupt inputs,
  unsafe paths and archive symlinks, named archive-member extraction,
  compressed GIFTI decoding, and smoke acceptance rejection of wrong hemisphere
  constants/labels, mask leakage, fractional labels, lost labels, and no coverage.
- `bash -n` and ShellCheck passed for the batch script. Source files are ASCII.
- Trillium SSH access was restored interactively. A live environment probe
  loaded `StdEnv/2023 connectomeworkbench/2.0.1` and ran `wb_command -version`.
  Filesystem free space at the probe: project 39 TB, scratch 17 TB (filesystem
  availability, not an account quota measurement).
- Created durable and scratch roots; uploaded the 39 MB runnable bundle with
  non-deleting rsync through remoteslurm. All 47 manifest entries passed remote
  SHA-256 verification.

Local retained evidence is under `work/`: `inputs/`, `fixtures-01/`, `bundle-01/`,
`trillium-environment-probe.json`, `trillium-create-workspace.json`,
`trillium-upload.log`, `trillium-preflight.json`, `trillium-queue.json`,
`trillium-quota.json`, and `ssh-probe.json`. Failed remote probes remain retained.

Bundle manifest SHA-256:

```
6a9e96bf662617916e6bded29bf4d2189f8b9ebe1c89ecca4e55a376adee22d1
```

Remote runnable directory:

```
/project/rrg-brad/brad/neuroatlas/surface-transforms-v1/bundles/6a9e96bf662617916e6bded29bf4d2189f8b9ebe1c89ecca4e55a376adee22d1/bundle-01
```

Scratch root:

```
/scratch/brad/neuroatlas/surface-transforms-v1
```

Blocker: the live scheduler interface did not return. Doctor's `squeue` and the
queue command's `sshare` each timed out at 60 seconds. The remote preflight
verified all 47 bundle files, then `sbatch --test-only` timed out within the
90-second command budget. Some remoteslurm stub startups also timed out at
45 seconds. SSH and the environment probe succeeded, so these failures do not
establish an authentication failure. The cause of the scheduler stalls is not
diagnosed. No batch job was submitted; there is no active job ID or readiness
pass. Do not run the resampling workload on the login node as a substitute.

Next action when the scheduler responds: rerun only submission preflight in
the existing verified bundle. From the local shell:

```sh
surface_bundle=/project/rrg-brad/brad/neuroatlas/surface-transforms-v1/bundles/6a9e96bf662617916e6bded29bf4d2189f8b9ebe1c89ecca4e55a376adee22d1/bundle-01
remoteslurm --no-daemon --json -H trillium run --timeout 90 \
  --cwd "$surface_bundle" 'sbatch --test-only trillium-smoke.sbatch'
```

Inspect the JSON response and require remote `rc: 0`. Only then submit:

```sh
remoteslurm --no-daemon --json -H trillium submit --remote \
  --cwd "$surface_bundle" "$surface_bundle/trillium-smoke.sbatch"
```

Retain the submission response and job ID, wait through the existing scheduler,
and retrieve/inspect `attempts/<job-id>/readiness.json`, GIFTIs, command log, and
`attempts/<job-id>-environment.txt`. A lost submission response requires job
recovery before another submit. Workbench compute execution and scientific
qualification remain unrun. Runtime integration, license review for asset
redistribution, and exact-template registration-fusion curation remain open.

## 2026-09-26 - Nibi readiness passed

Nibi's SSH session, scheduler queries, Workbench 2.0.1, and project/scratch
storage were reachable. The resolved project root is `/project/6005945`.
Preparation used account `def-brad_cpu` and an explicitly bounded allocation
of one CPU, 4 GB, and 15 minutes. The successful job ran in
`cpubase_bycore_b1` on `c629`.

Two RemoteSlurm transfer bugs were reproduced against the installed CLI:
upload source trailing slashes are stripped, and rsync omits configured SSH
transport options. See `remoteslurm-findings.md`. Transfers therefore used
explicit non-deleting rsync with the live Nibi ControlPath. Scheduler actions
continued through remoteslurm. Its initial local registry preflight was blocked
by sandbox permissions before submission; the authorized retry recorded the
job successfully. This local permission failure is separate from the two bugs.

The first Nibi bundle, manifest
`59cb6f757ab8559f7fcf7d04cdeab866641b687c84abaf1ac822e5789c7f298c`,
passed all 47 remote hashes and Slurm test-only. Job `22718987` ran for ten
seconds on `c644` and failed the label-membership check. Its output contained
unassigned key 1 because the fixture's background key 0 was named `label_0`.
Workbench resolves its unassigned label by the name `???` and creates a key
when that name is absent, as confirmed in the
[Workbench 2.0.1 source](https://github.com/Washington-University/workbench/blob/150de12f4f4b94b39bec6d9133ad2e7019d2d3ef/src/FilesBase/GiftiLabelTable.cxx#L630).

The fixture generator now writes transparent `???` at key 0. The runner also
validates the label table before any resampling, with a regression test for
the original error. Allowed output-label sets and numerical acceptance were
not relaxed. Six preparation tests pass. Fixture preparation was rerun into
`work/fixtures-02`; the original fixtures/bundle/job outputs are retained.

Successful bundle manifest SHA-256:

```
79948c5c261a30800f20348837c7c79b223c92a491c0ad1e1b7eed5fcb033f97
```

Remote bundle:

```
/project/6005945/neuroatlas/surface-transforms-v1/bundles/79948c5c261a30800f20348837c7c79b223c92a491c0ad1e1b7eed5fcb033f97
```

All 47 remote bundle hashes and test-only passed. Job `22719122` completed
with exit code 0 in 21 seconds. `attempts/22719122/readiness.json` reports four
passing cases; its 17 output/command-log hashes, input-lock hash, and bundle
manifest hash were verified after download. An independent R/gifti readback
also passed all four cases, including constant values on covered cortex,
target-mask exclusion, integer label membership, and coverage counts.

| Direction | Hemisphere | Covered target cortex | Target cortex | Max constant error |
|---|---|---:|---:|---:|
| fsaverage 164k -> fsLR 32k | L | 29401 | 29696 | 4.77e-7 |
| fsLR 32k -> fsaverage 164k | L | 148426 | 149955 | 4.77e-7 |
| fsaverage 164k -> fsLR 32k | R | 29393 | 29716 | 9.54e-7 |
| fsLR 32k -> fsaverage 164k | R | 148709 | 149926 | 9.54e-7 |

Labels were exactly from `{0, 11, 12}` on the left and `{0, 21, 22}` on the
right. No metric leaked through the target mask. Coverage is deliberately
reported rather than asserted complete; source and target masks differ.

Evidence under `work/`:

- `nibi-environment-probe.json`, `nibi-preflight-02.json`.
- `nibi-submission-03.json`, `nibi-wait-02.json`.
- `nibi-attempts-02/22719122/readiness.json` and all output GIFTIs/command logs.
- `nibi-attempts-02/22719122-environment.txt` (modules, executable hash, libraries).
- `nibi-independent-readback.json` (second-reader checks).
- `nibi-attempts-01/22718987/`, `nibi-job-22718987-output.json`, and original
  submission/wait receipts (retained failed attempt).
- `remoteslurm-bug-reproduction.json` (side-effect-free installed-CLI reproductions).

There are no active jobs from this preparation. The compute environment smoke
is complete. No surface runtime route is activated or scientifically qualified.
Next work is the domain/capability contract and candidate neurotransform
integration, followed by frozen acceptance criteria and independent numerical
and visual qualification. The earlier Trillium timeout cause remains unresolved;
the later direct SSH comparison failed authentication before reaching Slurm.

## Domain/capability foundation and engine gate, 2026-09-26

Implemented `surface_domain()` descriptors in `R/surface_domain.R`: template,
hemisphere, density, correspondence frame/revision, ordered coordinate and
triangle hashes, cortical-mask hash, and optional area hash/units. Triangle
indexing is explicit; descriptors store identities, not geometry arrays.
Typed planner endpoints preserve those identities and reject hemisphere
mismatches and modified descriptors. Same-name surface strings no longer imply
identity, including fsLR densities outside the current registry.

Corrected legacy surface registry entries to planned/approximate/non-reversible.
The manifest now exposes source/target representations and API execution
capability. Voxel routes remain volumetric, vertex routes remain on surfaces,
and projections are not automatically composed into multistep shortcuts. The
published V1 volume rows and artifacts are unchanged.

The local compiled neurotransform barycentric engine failed the analytic
face-order admission gate. An octahedral-sphere query gives +1/3 or -1/3 solely
depending on triangle row order, with opposite-side contributors. Exact probe,
receipt identity, source diagnosis, and repair acceptance are in
`check-surface-engine.R` and `neurotransform-findings.md`. Surface execution and
operator artifact caching remain blocked on a repaired, pinned engine and
method-matched independent qualification. No new route is activated. No new
Slurm job was needed or submitted for this phase.

Verification so far:

- `devtools::document()` regenerated only the relevant exports and help files.
- First focused pass: 147 assertions across surface domains, routing, and
  template-transform execution; zero failures/warnings/skips.
- After expanding unknown-density and typed-advisory coverage: 111 assertions
  across surface domains and routing; zero failures/warnings/skips.
- All four real locked mesh descriptors match the preparation's independent
  coordinate/topology hashes. Both hemisphere routes remain planned. Receipt:
  `work/domain-descriptors-01.json`.
- The full default test invocation aborted with signal 6 (exit 134) during
  atlas-reference tests, without an R traceback. It was not a timeout and was
  not killed by the agent. Receipt: `work/full-tests-domains-01.log.meta.json`.
- A clean archive of the unchanged HEAD reproduced the same signal-6 abort at
  the same atlas-reference progress point (27 assertions). Receipt:
  `work/baseline-atlas-ref-01.log.meta.json`; this establishes that abort predates
  the changes. The archive path is recorded in `work/baseline-path.txt`.
- The full invocation with `NOT_CRAN=false` used existing CRAN skip rules and
  progressed farther, then also aborted with signal 6 during cluster-explorer
  tests (60 assertions). Receipt:
  `work/full-tests-domains-offline-02.log.meta.json`. No full-suite pass or
  complete R CMD check is claimed.
- Six preparation tests still pass; `git diff --check` is clean. No commit or
  push was performed. No jobs or test processes remain running for this phase.

Next action: repair and independently test the upstream barycentric face
selection before operator integration. The domain/capability foundation is
ready for review; the broader Mote remains open with the numerical blocker.

## Upstream repair verification, 2026-09-26

Verified local and remote main at
`933edddda462593941e167726e8aaa7168ff103a`. Implementation files are identical
to the evidence-bound `9ab40ffd9b3b9fd46ac2e805826cbc8fde60b429` revision.
The rebuilt 0.2.0 candidate at `/tmp/neurotransform-surface-library` matches all
51 checked source, installed-artifact and evidence hashes. Evidence is retained
under `work/upstream-933eddd-binding.json`.

Fresh consumer checks using that isolated library:

- The unchanged `check-surface-engine.R` probe passes both face orders at +1/3.
  Receipt: `work/upstream-933eddd-consumer-probe.json`.
- Domain, planner and volume-transform suites: 154 assertions, zero failures,
  test warnings or skips. Log: `work/upstream-933eddd-consumer-tests.log`.

The original numerical defect is resolved. Upstream now supplies indexed shared
geometry, conservative mesh admission, masks/missingness and categorical
policies, adjoint/reverse contracts, and experimental adaptive area resampling.
Its recorded ordinary full-density construction times are 0.424-1.39 seconds;
this is upstream measured evidence, not a benchmark rerun here.

The remaining admission boundary is explicitly recorded in upstream
`output/surface-completion/qualification.md`: three of four ordinary comparisons
and adaptive upsampling comparisons fail the original 5e-5 metric tolerance.
Seventeen ordinary near-edge targets disagree with Workbench, while an
independent geometric solution agrees with native weights within 1.58e-14.
Do not relabel these failed gates as passes or expand tolerances retrospectively.
The scientific contract must either prospectively bound comparator disagreement
for the native method or qualify a separately named compatibility mode.

DESCRIPTION retains the existing production pin; no surface route was activated.
The historical failing receipts remain intact. No upstream changes, remote jobs,
global package installations, commit or push were performed by this verification.

## Native ordinary integration and qualification, 2026-09-27

Completed the explicit-domain consumer slice in R/surface_transform.R and pinned
DESCRIPTION to neurotransform 0.2.0 / 933eddd. Geometry and GIFTI value binding,
continuous/probability/label policies, directed operators, provenance, cache
ownership/integrity/offline behavior and selective cleanup are implemented.
The broad surface registry remains planned; arbitrary operators remain unqualified.

Frozen native-contract-v1.json SHA e9f217406649773f795a00a75cf0f5172986b6d3724408e4cc55681a2fed882d
precedes fresh fixtures. Final attempt work/integration-20260927/native-03 passes
84 synthetic + 1278 real independent exhaustive queries, all four full-density
invariant sets, and 392668-row policy accumulation. Max weight error 3.41e-14;
no sampled label/availability mismatch. Review found probability output roundoff
rejected at 1+2e-16; fixed with bounded output-only clipping and a deterministic
six-vertex regression. Source, engine, sampling and policy artifacts are now
hash-bound; tampered samples or policy bytes fail. Review closed all four findings.
Historical Workbench failures remain unchanged. See native-qualification.md and
qualification/native-ordinary-v1 for the scientific claim and durable receipts.

Final focused tests: 246 pass, no test warnings/failures/skips. R CMD check on a
source-identical staged build: 0 errors, 2 warnings, 0 notes; installed suite
1458 pass / 41 warnings / 95 skips. All 41 warning signatures reproduce on the
unchanged baseline. The old R abort was rgl OpenGL context creation; process-local
RGL_USE_NULL=true resolves it. A valid UTF-8 locale resolves metadata-check startup
errors. Existing CRAN incoming/URL and undeclared png-test dependency warnings
remain; manual/vignette generation and unavailable suggested packages are untested.

Retained intermediate attempts: native-01 was superseded by stronger evidence
binding and probability checks; native-02 had an overly exact constant assertion
(the frozen contract already specified 1e-12), corrected for native-03. The first
supplemental policy diagnostic was stopped after identifying quadratic named-list
lookup; the identical calculation with positional lookup passes in 12 seconds.
Initial package-check attempts exposed an obsolete wrapper argument, broken annex
links during R's pre-ignore copy, and invalid inherited locale; all are preserved.

No upstream edits, global install, new remote jobs, commit or push. Next decision:
review/land this explicit-domain slice, then design exact-domain qualified route
resolution and separately pursue population registration-fusion assets. Existing
volume release artifacts are unchanged. Main Mote stays open for the wider work.

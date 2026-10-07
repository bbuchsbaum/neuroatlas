# Resume neuroatlas on another machine

Updated 2026-10-06. Working branch: `master`.
Released: **[0.2.0](https://github.com/bbuchsbaum/neuroatlas/releases/tag/v0.2.0)**.
[PR #39](https://github.com/bbuchsbaum/neuroatlas/pull/39) is merged as
`fb2384a77f6905313f67247abafe14e2759cfb66`; `v0.2.0` points to that exact commit.
Publication was verified at 2026-10-07 00:46 UTC after all required checks passed.
Subsequent handoff/Mote changes do not change the released package code.
The broader transform Mote remains open for later coverage.

## Start from GitHub

```sh
git clone --branch master \
  https://github.com/bbuchsbaum/neuroatlas.git
cd neuroatlas
git status --short --branch
git log -1 --oneline
mkdir -p .mote/local .mote/tmp
mote actor set your-machine-actor
mote doctor
mote fsck
mote board
mote show bd-01M3CCBG9346NRV3WEVH5YNBCQ
```

Install the Mote CLI separately if absent. Do not run `mote init`: the shared
format and append-only operations are already Git-tracked. Set a distinct actor
for each machine. Actor identity, sessions, caches, downloads and temporary files
are local. Commit and push Mote changes with the code; no export/import step is
needed to reconstruct the board.

On this Google Cloud devbox, `/work` is a persistent disk. The Mote binary is
`/work/.local/bin/mote`, linked through `~/.local/bin/mote`. Home and Codex
configuration are on the persistent boot disk. Persistence across sessions does
not substitute for pushing shared code and operations to GitHub.

## Implemented release scope

The first coherent slice contains representation-safe planning, four exact
fsaverage 164k / fsLR 32k native directed routes, MNI6/MNI2009c population cortical
projection, and separately declared aligned white/pial ribbon sampling.
Broad surface aliases remain planned. Equal verified domains require a proven
diagonal unit-weight operator for exact index identity admission.

Read [the coverage plan](template-transform-coverage.md),
[release evidence](../data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md)
and [per-asset license audit](../data-raw/surface-transforms-v1/LICENSES.md).
Raw pinned inputs are fetched from their original providers and are not bundled
by this slice. Population interpolation does not establish anatomical accuracy,
subject registration, area conservation or an inverse. Historical strict
Workbench failures and their thresholds remain retained.

## Rebuild the environment

Use R >= 4.1, an R-compatible C++ toolchain and dependencies in DESCRIPTION.
The devbox validated core code on Ubuntu 24.04 / R 4.3.3, neuroim2 0.19.1,
neurosurf 0.1.0.9004 and neurotransform 0.2.0. The engine is pinned to
`933edddda462593941e167726e8aaa7168ff103a`. Roxygen2 is pinned to 7.3.3 for
reproducible generated documentation. Do not copy another machine's binary hashes.

```sh
export NEUROATLAS_DEV_LIBRARY="$PWD/data-raw/surface-transforms-v1/work/library"
mkdir -p "$NEUROATLAS_DEV_LIBRARY"
export R_LIBS="$NEUROATLAS_DEV_LIBRARY"
export RGL_USE_NULL=true
export R_USER_CACHE_DIR="$PWD/data-raw/surface-transforms-v1/work/cache"
export LC_ALL=C.UTF-8  # choose an installed UTF-8 locale on your machine
export OPENBLAS_NUM_THREADS=1
export OMP_NUM_THREADS=1
Rscript -e 'install.packages(c("remotes", "devtools"), repos="https://cloud.r-project.org")'
Rscript -e 'remotes::install_deps(".", dependencies=TRUE, upgrade="never")'
Rscript data-raw/surface-transforms-v1/setup-engine.R "$NEUROATLAS_DEV_LIBRARY"
export NEUROATLAS_ENGINE_BINDING="$NEUROATLAS_DEV_LIBRARY/engine-build.json"
python3 data-raw/surface-transforms-v1/test-engine-binding.py \
  "$NEUROATLAS_ENGINE_BINDING"
Rscript -e 'devtools::test(stop_on_failure=TRUE)'
Rscript -e 'devtools::check(manual=TRUE)'
```

The engine setup verifies the published source archive SHA-256 and records the
fresh installation's artifacts. Optional packages, downloads, graphics, LaTeX
and qpdf have separate requirements. Report warnings and skips explicitly.

The local devbox environment is retained in the ignored
`data-raw/surface-transforms-v1/work/devbox-env.sh`. Its absolute paths and Mote
session belong to this machine; create your own environment elsewhere.

## Reproduce scientific qualification

Follow [native qualification](../data-raw/surface-transforms-v1/native-qualification.md)
and [the projection protocol](../data-raw/surface-transforms-v1/projection-v1/README.md).
Use a fresh output directory for every attempt and serialize jobs sharing a
cache. Do not edit sources, registry or manifests while a consumer driver runs.

```sh
python3 data-raw/surface-transforms-v1/fetch-inputs.py \
  data-raw/surface-transforms-v1/work/inputs
export NEUROATLAS_SURFACE_INPUTS="$PWD/data-raw/surface-transforms-v1/work/inputs"
export NEUROATLAS_SURFACE_FIXTURES="$PWD/data-raw/surface-transforms-v1/work/fixtures-new"
Rscript data-raw/surface-transforms-v1/prepare-fixtures.R \
  "$NEUROATLAS_SURFACE_INPUTS" "$NEUROATLAS_SURFACE_FIXTURES"
python3 data-raw/surface-transforms-v1/test-preparation.py
Rscript data-raw/surface-transforms-v1/qualify-native.R \
  data-raw/surface-transforms-v1/work/native-new
python3 data-raw/surface-transforms-v1/qualify-native-oracle.py \
  data-raw/surface-transforms-v1/work/native-new
Rscript data-raw/surface-transforms-v1/qualify-native-policies.R \
  data-raw/surface-transforms-v1/work/native-new
python3 data-raw/surface-transforms-v1/test-native-evidence.py \
  data-raw/surface-transforms-v1/work/native-new
```

Independent references use Python NumPy 2.4.3, SciPy 1.18.1, nibabel 5.4.2 and
SimpleITK 2.5.3, plus Workbench 1.5.0. Matplotlib 3.11.2 is for local QA figures.
Python is not required by the public R API. The projection protocol describes
additional checksum-locked reference inputs and all reproduction commands.

## Current engineering and review status

Full development tests: **2,770 passes, zero failures, 44 warnings and two opt-in
skips**. Full R CMD check with PDF manual: **zero errors, zero warnings, three
notes**. Installed tests, examples and vignette rebuilds pass. Notes concern
installed size, time verification and unavailable optional HTML validation.
A documentation-only follow-up verifies the slow cache example's `donttest`
annotation; all executable R expressions match the full-check snapshot.
The website build and project lint gate pass. Workflow syntax and seven offline
release-guard tests pass. Candidate `63a4334` passes hosted project, OS and
website checks. The merged tree is identical to that validated candidate.
Exact-SHA default-branch verification at `fb2384a` also passes:

- [Project checks and authenticated coverage delivery](https://github.com/bbuchsbaum/neuroatlas/actions/runs/37550526551).
- [OS matrix](https://github.com/bbuchsbaum/neuroatlas/actions/runs/37550526258):
  Windows, macOS, R-devel and R-oldrel all report `Status: OK`.
- [Website build and deployment](https://github.com/bbuchsbaum/neuroatlas/actions/runs/37550526317).
- [Guarded publication](https://github.com/bbuchsbaum/neuroatlas/actions/runs/37553675097).

Codecov project and patch statuses succeed on the same merge commit. The live
[website](https://bbuchsbaum.github.io/neuroatlas/) and projection reference page
serve version 0.2.0. A fresh GitHub clone verifies 202 pre-closeout Mote operations,
all 54 retained evidence files, and each scientific receipt's 73 source bindings.
Final closeout operations are committed with this handoff; reconstruct the latest
board rather than treating that pre-closeout operation count as current.

The owner selected neuroatlas conventions for `.lintr`, with complexity reviewed
[separately](complexity-review.md). Mandatory project CI checks documentation,
lint, package behavior, website building and coverage measurement. rOpenSci
reports remain visible and advisory. Eco-atlas is unused and manual-only; its
old API-key failure is not a current package gate.

Code coverage delivery uses Codecov OIDC authentication and fails CI on upload
errors. This replaces the unauthenticated upload at `dfe103e`, whose rejection
was hidden by the action's default behavior. The corrected candidate's and
merged commit's coverage reports were accepted for processing. Coverage artifacts
are also retained in GitHub Actions, independently of the external dashboard.

The release workflow can publish only approved version 0.2.0 from the current
default-branch commit after exact-SHA project, OS and website checks pass. It
rejects fork/PR/old/pending/failed checks and never replaces an existing tag.

Numerical gates pass on the final source bindings: 1,362 native oracle queries,
maximum weight discrepancy `3.397282455352979e-14`; 108 projection cases, maximum
scalar discrepancy `2.842170943040401e-14`; zero label/availability mismatches.
Five AI-inspected local figures have hashed inputs; all 20 binary plotting inputs
match the current public examples. Human review and held-out anatomical accuracy
are not established.

## Work remaining beyond this slice

CIFTI with preserved subcortex, directed surface-to-volume rasterization, other
surface densities, CIVET, specialist templates and further anatomical validation
remain separate work under `bd-01M3CCBG9346NRV3WEVH5YNBCQ`. The quality-workflow
Mote `bd-01M47HZ11AWSD23BCAB6NXNXGY` is closed after exact-SHA verification and
publication. The broad transform Mote remains open for the work listed above.
This devbox's claims, reservations and session are released at closeout.
Do not reuse leases from another machine.

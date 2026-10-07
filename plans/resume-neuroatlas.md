# Resume neuroatlas on another machine

Updated 2026-10-06. Working branch: `feat/native-surface-transforms`.
Release candidate: **0.2.0**. This handoff records local validation; consult Mote
and GitHub for subsequent hosted checks, merge and publication status.
The broader transform Mote remains open for later coverage.

## Start from GitHub

```sh
git clone --branch feat/native-surface-transforms \
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
release-guard tests pass. Exact-SHA hosted verification remains a separate step.

The owner selected neuroatlas conventions for `.lintr`, with complexity reviewed
[separately](complexity-review.md). Mandatory project CI checks documentation,
lint, package behavior, website building and coverage measurement. rOpenSci
reports remain visible and advisory. Eco-atlas is unused and manual-only; its
old API-key failure is not a current package gate.

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
Mote is `bd-01M47HZ11AWSD23BCAB6NXNXGY`; consult its latest state and hosted run
links. Do not reuse active leases from another machine.

# Resume neuroatlas on another machine

Updated 2026-10-06. Working branch: `feat/native-surface-transforms`.
Mainline integrated through `f29dcc54911ac76548fe82b52b63b65afe865eb5`.
Mote is authoritative; its shared format and append-only operations are tracked.
Actor identity, leases, temporary files, input downloads and operator caches are
local. A clean Git checkout does not imply the scientific release is complete.

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
mote board
mote show bd-01M3CCBG9346NRV3WEVH5YNBCQ
```

Install the Mote CLI separately if absent; do not initialize a second store.
Choose a distinct actor on the new machine. Use `mote begin` with the exact paths
of the next bounded task. Do not reuse another machine's active reservations.

## Rebuild the development environment

Use R >= 4.1 and an R-compatible C++ toolchain; validation here used R 4.5.1.
The package's DESCRIPTION is the dependency manifest. The surface engine is
pinned to `933edddda462593941e167726e8aaa7168ff103a`; the setup script additionally
checks the downloaded source archive SHA-256 and records the new build's hashes.
Binary hashes are machine-specific and must not be borrowed from old receipts.

```sh
export NEUROATLAS_DEV_LIBRARY="$PWD/data-raw/surface-transforms-v1/work/library"
mkdir -p "$NEUROATLAS_DEV_LIBRARY"
export R_LIBS="$NEUROATLAS_DEV_LIBRARY"
export RGL_USE_NULL=true
export R_USER_CACHE_DIR="$PWD/data-raw/surface-transforms-v1/work/cache"
# Select an installed UTF-8 locale, e.g. en_US.UTF-8 on macOS.
Rscript -e 'install.packages(c("remotes", "devtools"), repos="https://cloud.r-project.org")'
Rscript -e 'remotes::install_deps(".", dependencies=TRUE, upgrade="never")'
Rscript data-raw/surface-transforms-v1/setup-engine.R "$NEUROATLAS_DEV_LIBRARY"
export NEUROATLAS_ENGINE_BINDING="$NEUROATLAS_DEV_LIBRARY/engine-build.json"
python3 data-raw/surface-transforms-v1/test-engine-binding.py \
  "$NEUROATLAS_ENGINE_BINDING"
Rscript -e 'devtools::test(stop_on_failure=TRUE)'
Rscript -e 'devtools::check()'
python3 data-raw/surface-transforms-v1/test-preparation.py
```

Optional packages, downloads, graphics, manual and vignette checks have separate
requirements. Report skips and warnings; do not count them as passes.

## Reproduce the scoped numerical qualification

The retained September evidence is a historical snapshot, including then-current
source hashes and failed Workbench comparisons. It is not a receipt for today's
entire package. `qualification/resume-20261006/` holds the refresh evidence.
The portable binding mode verifies a fresh engine build without requiring an
old sibling checkout or identical binaries. The original binding mode remains
available for replaying the historical environment.

Use Python with NumPy; the prior oracle used NumPy 2.4.3. Every destination below
must be new. Inputs are fetched from the checksum lock, not redistributed here.

```sh
python3 -m venv data-raw/surface-transforms-v1/work/python
data-raw/surface-transforms-v1/work/python/bin/pip install numpy==2.4.3
python3 data-raw/surface-transforms-v1/fetch-inputs.py \
  data-raw/surface-transforms-v1/work/inputs
export NEUROATLAS_SURFACE_INPUTS="$PWD/data-raw/surface-transforms-v1/work/inputs"
export NEUROATLAS_SURFACE_FIXTURES="$PWD/data-raw/surface-transforms-v1/work/fixtures-new"
Rscript data-raw/surface-transforms-v1/prepare-fixtures.R \
  "$NEUROATLAS_SURFACE_INPUTS" "$NEUROATLAS_SURFACE_FIXTURES"
Rscript data-raw/surface-transforms-v1/qualify-native.R \
  data-raw/surface-transforms-v1/work/native-new
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/qualify-native-oracle.py \
  data-raw/surface-transforms-v1/work/native-new
Rscript data-raw/surface-transforms-v1/qualify-native-policies.R \
  data-raw/surface-transforms-v1/work/native-new
data-raw/surface-transforms-v1/work/python/bin/python \
  data-raw/surface-transforms-v1/test-native-evidence.py \
  data-raw/surface-transforms-v1/work/native-new
```

## Work remaining

Read [the coverage plan](template-transform-coverage.md) and
[native qualification](../data-raw/surface-transforms-v1/native-qualification.md).
The broader surface/projection Mote remains open. Review exact-domain route
admission and per-asset redistribution licenses next; keep broad aliases planned.
Registration fusion, aligned ribbon sampling, CIFTI and directed backprojection
remain separate implementation/qualification work. Preserve the failed strict
Workbench gates; native correctness does not establish anatomical accuracy,
Workbench equivalence, reversibility or area conservation.

## GitHub verification boundaries

Remaining quality-workflow Mote: `bd-01M47HZ11AWSD23BCAB6NXNXGY`.

Current local verification: 2,672 development assertions pass, zero failures,
45 warnings and five optional/opt-in skips. R CMD check with `--as-cran
--no-manual` and `_R_CHECK_FORCE_SUGGESTS_=false` returns zero errors, one warning
and zero notes; examples, installed tests and vignette rebuilds pass. The warning
is CRAN incoming feasibility (development version/Remotes, non-CRAN dependencies,
two Princeton URLs returning 403 and tarball size). PDF manual generation is
unverified. Seven optional suggested packages were unavailable. The previous
sandbox-cache failure is retained separately and is not a code regression.

The portable native rerun passes the frozen geometric gates: 1,362 independent
oracle queries, maximum weight error `3.4083846855992306e-14`, and zero sampled
categorical/availability mismatches. Preparation has six passing offline tests.
Do not extend this exact-input/method evidence to the unfinished release scope.

The latest mainline R-CMD-check-OS run passes. Existing other mainline jobs fail:

- Checklist crashes in its external linter organisation lookup, and previously
  reported undeclared `png` in CPU plot tests. `png` is now declared in Suggests.
- Pkgcheck reports missing contributing guidance, examples/return docs, reference
  grouping, unexpected files and its default-branch naming policy.
- Eco-atlas receives HTTP 401 from its configured OpenAI API secret. The owner
  must replace that GitHub Actions secret before that workflow can pass.

The feature branch now triggers the existing OS matrix as well as its existing
checklist workflow. Do not describe the whole repository as green while these
failures remain. Preserve hosted run URLs and exact SHAs in the Mote handoff.

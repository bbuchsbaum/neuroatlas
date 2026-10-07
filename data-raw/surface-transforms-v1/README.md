# Surface transform preparation

This workspace prepares the fsaverage 164k <-> fsLR 32k slice, separately for
left and right hemispheres. It does not activate a neuroatlas route or change
the volumetric V1 release. Passing the Workbench smoke test establishes that
the pinned inputs and reference executable work together; it does not qualify
neurotransform or establish anatomical accuracy.

## Inputs and provenance

`inputs.lock.json` pins 28 files (about 38 MB) with SHA-256 and byte counts:

- TemplateFlow fsaverage at `8e53ba4f2e438758f69d11436fe0cd291a28bec6`.
- TemplateFlow fsLR at `ca545b4721c2858decef9bbca302c1eac4d0d8bf`.
- fsaverage masks and reference spheres from the neuromaps 164k archive,
  whose published MD5 comes from neuromaps commit
  `ffcc2e0f657943ce00a1b6a968396f32250e495c`. The archive is additionally SHA-256
  pinned. Only named regular files are read from it.

TemplateFlow asset downloads were checked against the size and MD5 in the
git-annex pointers at those revisions before their SHA-256 was recorded.
Mutable download URLs are acceptable only when the downloaded bytes match
the lock. Existing corrupt files are rejected, never silently replaced.

The correspondence pair is the fsaverage native sphere and the fsLR sphere
explicitly in fsaverage space. The native fsLR sphere is not interchangeable
with the registered sphere. The fsaverage `desc-std` sphere is retained for
comparison, but is not selected for this smoke: the released neuromaps sphere
matches the native TemplateFlow sphere's coordinates and triangle ordering
exactly in both hemispheres; `desc-std` coordinates differ slightly.

The fsaverage masks are absent from the inspected TemplateFlow catalog. The
neuromaps archive packages each mask with its reference sphere. Preparation
checks exact reference-sphere array equality, not just vertex counts, before
using the mask. These checks do not independently prove mask anatomy.

Both TemplateFlow template descriptions say `See LICENSE file`, but neither
pinned repository contains that file. The neuromaps archive has no license
member. Preserve source provenance and complete per-asset license review
before redistribution. Nothing in this workspace relicenses the assets.

## Local preparation

Run from the neuroatlas repository root. Python uses the standard library;
fixture preparation uses the installed R packages `gifti`, `digest`, and
`jsonlite`. Each fixture and bundle destination must be new.

```sh
python3 data-raw/surface-transforms-v1/fetch-inputs.py \
  data-raw/surface-transforms-v1/work/inputs
Rscript data-raw/surface-transforms-v1/prepare-fixtures.R \
  data-raw/surface-transforms-v1/work/inputs \
  data-raw/surface-transforms-v1/work/fixtures-01
python3 data-raw/surface-transforms-v1/test-preparation.py
python3 data-raw/surface-transforms-v1/build-bundle.py \
  data-raw/surface-transforms-v1/work/inputs \
  data-raw/surface-transforms-v1/work/fixtures-01 \
  data-raw/surface-transforms-v1/work/bundle-01
```

The preparation validates byte hashes, vertex/face counts and indices, identical
ordered topology across corresponding anatomical/registration meshes, finite
coordinates, positive area metrics, and binary masks with both included and
excluded vertices. It records ordered vertex and topology hashes in
`fixtures/domains.json`. The hash representation is documented in
`prepare-fixtures.R`; it is a preparation contract, not a new public R class.

Smoke fixtures contain hemisphere-specific constants (7 left, 13 right),
binary cortical masks, and integer labels (11/12 left, 21/22 right; 0 background).
All generated GIFTIs are reopened and checked locally. `R-session.txt` records
the actual fixture-generation environment.

`work/` holds downloaded assets, generated fixtures, bundles, logs, and remote
receipts and is ignored by Git. The lock and preparation scripts are tracked
source candidates. The workspace is excluded from R package builds by the
existing `^data-raw$` rule.

## Trillium execution

Use `remoteslurm --no-daemon --json -H trillium` for cluster operations. Refresh
connection, account/partition limits, and storage before submission. The smoke
requests one CPU, 4 GB, 15 minutes, `def-brad`, partition `compute`.

Durable bundles and receipts belong below
`/project/rrg-brad/brad/neuroatlas/surface-transforms-v1/`. Future large temporary
operators and caches belong below `/scratch/brad/neuroatlas/`; scratch is not
the only copy of provenance or inputs. The small smoke writes its outputs to
the durable bundle's `attempts/<Slurm job ID>/` directory.

Stage each bundle into a new directory identified by the full SHA-256 of its
`SHA256SUMS` file. Transfer without deletion. Check all manifest entries on the
cluster, then run `sbatch --test-only trillium-smoke.sbatch` from that directory.
The current `remoteslurm put` converts its source to a `Path` and strips the
trailing slash: uploading `bundle-01/` into an existing hash directory creates
`<hash>/bundle-01/`. Use that actual directory as the job's working directory.
Only submit after preflight succeeds. Submit using `remoteslurm submit` with
that same `--cwd`; the script depends on `SLURM_SUBMIT_DIR` being the bundle.
Save the submission response before waiting. A timeout or lost response requires
checking for an existing job before any retry.

The script loads `StdEnv/2023` and `connectomeworkbench/2.0.1`, fixes OpenMP to one
thread, verifies all bundle hashes, and uses no compute-node downloads. It
records module state, Workbench version, executable SHA-256 and linked
libraries. Its Python reader has no NumPy/nibabel dependency.

For both directions and hemispheres it exercises Workbench `ADAP_BARY_AREA`
metric and label resampling with average vertex-area metrics and a source ROI.
It explicitly masks the output metric with the target cortical ROI and retains
the valid-source coverage map. A readiness pass requires expected output size,
finite values, nonempty cortical coverage, constant error <= 1e-5 on covered
cortex, no target-mask leakage, and integer labels from the source label set.
Label output is retained unmasked for diagnostic inspection. Coverage counts
are diagnostics; this smoke does not impose a scientific coverage threshold.

Use `remoteslurm wait <job-id> --timeout 900 --poll 30`, then retrieve the
attempt directory and its separate environment receipt. Inspect
`readiness.json`, the actual output GIFTIs, and `commands.jsonl`; a Slurm
`COMPLETED` state alone is insufficient. Failed attempts remain in place.

## Nibi execution

Nibi uses the same pinned inputs, fixtures, Workbench version, and smoke logic.
Build with `build-bundle.py ... --cluster nibi` to include `nibi-smoke.sbatch`.
The tested allocation is `def-brad_cpu`, partition `cpubase_bycore_b1`, one CPU,
4 GB, 15 minutes. The durable root is
`/project/6005945/neuroatlas/surface-transforms-v1/`; scratch is
`/scratch/brad/neuroatlas/surface-transforms-v1/`.

Current remoteslurm rsync-backed transfers omit configured SSH transport
options as well as stripping the upload source slash. For this run, transfer
used explicit non-deleting rsync with the live Nibi ControlPath, while all
scheduler operations used remoteslurm. See `remoteslurm-findings.md` for the
reproductions and `work-log.md` for exact bundle and job receipts. Refresh the
live connection settings before future transfers; do not assume an old socket
remains authenticated.

Pass the resource options explicitly to `remoteslurm submit` as well as keeping
them in the script: `-A def-brad_cpu -p cpubase_bycore_b1 -t 00:15:00
-o nodes=1 -o ntasks=1 -o cpus-per-task=1 -o mem=4G`. RemoteSlurm host defaults
otherwise can override `#SBATCH` directives, including the partition.

Fixture label 0 is transparent and named `???`, Workbench's unassigned-label
convention. The runner checks this before resampling. The initial Nibi attempt
exposed the missing convention in the original fixture; it is retained as a
failed attempt, and label-value acceptance was not relaxed.

## Native ordinary integration

For a fresh machine, follow [the resume guide](../../plans/resume-neuroatlas.md).
`setup-engine.R` builds the checksum-pinned engine in an isolated library and
records its installed hashes. Set `NEUROATLAS_ENGINE_BINDING` to that build
receipt to run `qualify-native.R` without the historical sibling checkout.
`NEUROATLAS_SURFACE_INPUTS` and `NEUROATLAS_SURFACE_FIXTURES` select independently
prepared input/fixture directories. The historical qualification script is
retained as `qualification/native-ordinary-v1/qualify-native-20260927.R`.
The September evidence index binds that historical source state; the refreshed
source and evidence are recorded separately under `qualification/resume-20261006/`.

The repaired neurotransform 0.2.0 revision `933eddd` is now pinned in DESCRIPTION.
Explicit-domain geometry, GIFTI values/labels, directed native resampling and a
verified operator cache are implemented. Fresh method-specific numerical gates
pass for all four pinned fsaverage 164k / registered fsLR 32k routes.
See [native qualification](native-qualification.md) for the frozen contract,
independent oracle, coverage, evidence hashes and reproducible commands.

The original Workbench parity failures remain failures. Adaptive resampling is
not exposed. Caller-supplied operators remain unqualified and broad named-space
surface routes remain planned; exact domain/method evidence is not interchangeable
with a generic template alias. Registration fusion, aligned ribbon sampling and
directed backprojection remain subsequent work.

Reference methods:

- [Workbench metric resampling](https://www.humanconnectome.org/software/workbench-command/-metric-resample)
- [Workbench label resampling](https://www.humanconnectome.org/software/workbench-command/-label-resample)
- [Pinned neuromaps surface selection](https://github.com/netneurolab/neuromaps/blob/ffcc2e0f657943ce00a1b6a968396f32250e495c/neuromaps/transforms.py)

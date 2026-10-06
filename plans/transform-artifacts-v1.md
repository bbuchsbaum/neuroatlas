# Neuroatlas template-transform artifacts v1

Status: V1 implemented and released; retained original implementation plan
Prepared: 2026-09-19
Release name: `transform-artifacts-v1`

2026-10-06: the registry advertises the qualified V1 volume artifacts, and the
public cache/application API is implemented. This document preserves the
original prospective plan; use [the current coverage plan](template-transform-coverage.md)
and [machine handoff](resume-neuroatlas.md) for remaining work.

## 1. Outcome

Produce, qualify, distribute, and expose nonlinear transforms between the
volumetric template spaces used by `neuroatlas` without adding large binaries
to the Git repository or the R package.

The first release will provide both directions between:

- `MNI152NLin6Asym`
- `MNI152NLin2009cAsym`

The release will contain neuroatlas-built ANTs composite transforms, their
provenance, checksums, and compact qualification evidence. Official
TemplateFlow transforms for the same pair will be retained as an independent
benchmark and fallback provider; they will not be copied into the Git history.

Success means that a user can ask `neuroatlas` for a source-to-target route,
download verified artifacts into a local cache, and transform an image or atlas
onto an explicit target grid with the correct interpolation and recorded
provenance.

### Verified implementation baseline

This plan is based on the 2026-09-19 checkouts, not the older June blocker
list:

- `niflowr` `main` at `3fe3493` provides the staged `ants.registration`
  renderer, `ni_ants_register_to_template(preset = "precise")`, composite and
  inverse H5 outputs, pinned container execution, provenance, and registration
  QA. Its focused template/transform-build tests passed 106 assertions with no
  failures, warnings, or skips in this checkout.
- `neurotransform` `main` at `9e7550d` loads TemplateFlow/ANTs H5 transforms,
  preserves corrected ITK/ANTs direction and LPS/RAS conventions, composes and
  inverts routes, transforms landmarks, computes Jacobians, and resamples
  `NeuroVol` objects.
- The installed pure-R `templateflow` 0.1.0 inventory exposed 26 transform rows,
  including official H5 files in both directions for the V1 MNI pair. Treat
  that count as an inventory snapshot and regenerate it during WP0.

## 2. Decisions

1. Large `.h5` transforms are GitHub Release assets, not Git objects, Git LFS
   objects, or package data. Each current warp is about 205 MB, above GitHub's
   100 MiB ordinary-Git limit but below the 2 GiB release-asset limit.
2. `transform-artifacts-v1` is immutable after publication. A changed transform,
   recipe, input template, or qualification policy requires a new artifact
   release. Never replace an asset at the same URL.
3. `niflowr` is a build-time tool. It runs pinned ANTs, writes execution
   provenance, and performs the first QA pass. It is not a runtime dependency
   of `neuroatlas`.
4. The expensive build and qualification stages run on Nibi through a durable
   `remoteslurm` campaign workspace. Scheduler completion is not artifact
   completion; campaign output contracts and qualification receipts are the
   remote acceptance boundary.
5. `neurotransform` is the optional runtime apply engine. It loads ANTs H5,
   composes routes, transforms points, and resamples `NeuroVol` objects.
6. One ANTs registration produces an oriented pair: `Composite.h5` and
   `InverseComposite.h5`. Do not fit the reverse direction independently unless
   inverse qualification fails.
7. The graph is sparse. For `N` compatible adult templates, add at most `N - 1`
   directly fitted hub edges before considering special direct edges. Do not
   build `N * (N - 1)` directed registrations.
8. Compose every multi-edge route in physical coordinates and resample the data
   once on the requested target grid. Never resample an image at every hop.
9. A completed ANTs command is not a qualified transform. An artifact becomes
   `available` only after byte, direction, geometry, anatomical, inverse, and
   deformation gates all pass.

## 3. Scope and non-goals

### V1 scope

- One fitted pair, yielding the two directed MNI routes above.
- 1 mm template images as registration inputs.
- Qualification on both 1 mm and 2 mm target grids.
- Scalar volumes, integer label atlases, and probabilistic atlases.
- Download/cache, route resolution, application, and provenance in
  `neuroatlas`.
- Linux/ANTs build execution in a pinned container. The H5 results are
  platform-independent.

### V1 non-goals

- An all-pairs TemplateFlow transform collection.
- Subject-to-template registration in `neuroatlas`.
- Surface-to-volume or surface-to-surface transforms.
- EPI registration.
- Runtime transform fitting.
- Treating `MNI152`, `MNI152_custom`, or unknown atlas provenance as if it were
  one of the two qualified nonlinear template spaces.

## 4. Efficient graph strategy

### V1 edge

Run one registration with:

- moving/source: `MNI152NLin6Asym`
- fixed/target: `MNI152NLin2009cAsym`
- output forward: NLin6Asym to 2009cAsym
- output inverse: 2009cAsym to NLin6Asym

This produces both directed assets for the cost of one fit.

### Expansion policy

For later adult templates, use `MNI152NLin2009cAsym` as the provisional adult
hub because it is already the native space of Glasser and HCPex in
`neuroatlas`. Re-evaluate the hub before adding a different population or age
domain. Pediatric, infant, disease-specific, and species templates must not be
routed through the adult hub merely to reduce compute.

Before fitting a new edge:

1. Inventory official TemplateFlow `suffix = "xfm"` resources.
2. If an official direct transform exists, qualify and use it rather than
   refitting by default.
3. Fit a new edge only when no acceptable official route exists, or when a
   neuroatlas-controlled artifact is an explicit release goal.
4. Prefer a qualified direct edge. Otherwise choose the fewest-hop compatible
   route with the best qualification tier.
5. Add a special direct edge only when the composed hub route fails a frozen
   quality or distortion gate.

This makes compute grow linearly with supported template families while still
allowing arbitrary graph routes.

## 5. Frozen inputs and recipe

Create a tracked build surface under:

```text
data-raw/transform-artifacts-v1/
  routes.json
  qualification-policy.json
  campaign.nibi-smoke.toml
  campaign.nibi.toml
  campaign-work-log.md
  build.R
  qualify.R
  assemble-release.R
  scripts/run-build.sh
  scripts/run-qualification.sh
  scripts/validate-build-output.R
  scripts/validate-qualification-output.R
  README.md
```

Generated attempts and downloaded images go under an ignored work root, never
under `inst/`:

```text
data-raw/transform-artifacts-v1-work/
  inputs/
  attempts/<pair-id>/<attempt-id>/
  release/
```

`routes.json` must pin for every pair:

- exact source and target TemplateFlow identifiers;
- cohort, resolution, suffix, descriptor, and extension queries;
- the resolved TemplateFlow relative paths;
- SHA-256 and byte size of T1w images and masks;
- TemplateFlow package and inventory revision information;
- registration preset and overrides;
- random seed, thread count, engine, and timeout;
- ANTs version and container image digest;
- expected output names and directions.

Use brain-extracted `res-01` T1w images and matching masks. Preserve the source
headers and physical coordinate systems; do not silently reorient, crop, or
rewrite them before registration. If preprocessing is necessary, it becomes a
named, checksummed recipe step.

The attempt ID is a digest of the route definition, input digests, preset
contents, ANTs container digest, and build-script revision. If an attempt with
that identity has complete verified outputs, the builder skips it. A failed or
partial attempt is retained with its logs and cannot be mistaken for a release
candidate.

## 6. Registration execution with niflowr

Use the current `niflowr` high-level surface:

```r
niflowr::ni_ants_register_to_template(
  fixed_image = target_brain_t1w,
  moving_image = source_brain_t1w,
  output_prefix = attempt_prefix,
  preset = "precise",
  fixed_image_mask = target_brain_mask,
  random_seed = 1,
  .engine = "apptainer",  # or pinned Docker in CI/local builds
  .profile = "ants",
  timeout = build_timeout,
  echo = TRUE
)
```

The current precise wrapper exposes ANTs' fixed-image metric mask. The moving
input is already brain-extracted; its separate mask is retained for QA rather
than passed as an unsupported registration argument. If paired registration
masks become necessary, add and test that capability in `niflowr` before
changing the frozen recipe.

The landed `precise` preset is the conservative V1 starting point. Although it
costs roughly 17 minutes with eight threads for a 1 mm T1w-to-template fit, V1
has only one fit; engineering a faster schedule before establishing a valid
reference would cost more than it saves. A later reduced schedule may replace
it only after it passes the same frozen gates.

Execution requirements:

- pin the ANTs container by registry digest, not a mutable tag;
- record the exact `niflowr`, ANTs, R, and build-script revisions;
- set an explicit seed and thread count;
- give every attempt its own directory;
- preserve stdout, stderr, rendered argv, runtime, and `niflowr` provenance;
- require both composite files and the warped QC image;
- hash outputs before renaming or copying them;
- run independent template pairs concurrently, bounded by measured memory;
- on a cluster, submit one pair per job-array element and publish only from a
  separate reconciliation step.

### Nibi execution through remoteslurm

Use `remoteslurm` campaign workspaces rather than ad hoc SSH or bare `sbatch`.
The current local `remoteslurm` checkout is `83592af`; its campaign layer
provides immutable definitions, preflight receipts, durable unit reservations,
attempt identity, bounded recovery, output settling, validators, and immutable
validation receipts.

This workload is also a deliberate real-site acceptance test of the new
campaign feature on Nibi. Keep campaign-mechanics evidence distinct from the
scientific qualification result: a successful synthetic canary proves the
campaign path exercised on Nibi, while only the production qualification proves
the template transforms.

Nibi is not yet described in the local `remoteslurm` configuration and no live
MFA session was available while this plan was written. Before writing resource
or path values into `campaign.nibi.toml`:

1. Run `remoteslurm connect nibi` in a user terminal.
2. Run `rslurm --json -H nibi info` and record its `notes`, `templates`,
   `projects`, environment values, and learned policy notes.
3. Add a bounded Nibi host entry and named CPU/ANTs template using the actual
   account, partition, walltime rules, modules, Apptainer availability, and work
   storage reported by Nibi. Do not copy Trillium policy into Nibi.
4. Configure one sync project for the build definition/scripts and a separate
   results mapping for qualified outputs. Both default to `delete = false`.
5. Preview each first transfer with `rslurm sync <project> --dry-run`.

Before the production definition, run `campaign.nibi-smoke.toml` in a separate
remote and output root. It uses tiny deterministic synthetic NIfTI inputs and
the `niflowr` testing preset to exercise:

- local campaign compilation and immutable definition identity;
- project synchronization and script identity;
- Nibi preflight and environment/tool resolution;
- one managed submission and attempt reservation;
- declared composite/inverse/provenance outputs;
- settling, SHA-256, command validation, and immutable receipts;
- bounded status/diagnosis;
- remote-to-local result transfer and checksum reconciliation;
- close/reopen-for-observation behavior without authorizing new work.

The smoke outputs are explicitly `production_evidence = false` and can never be
copied into the release bundle. Production starts only after the canary is
reconciled or any remoteslurm defect has a disposition.

The campaign has two remote stages:

1. `build`: one unit per undirected template pair. It runs the pinned
   `ni_ants_register_to_template()` recipe and declares the forward H5, inverse
   H5, warped QC image, build provenance, and build receipt as outputs.
2. `qualify`: depends on a **verified** build unit. It runs the heavy ANTs QA,
   landmark, direction, grid, label, probability, and Jacobian checks and emits
   compact machine-readable QA plus a rendered report.

Use array execution for both stages so later pairs can run independently.
For V1 the inventory has one pair, so it produces one build allocation and one
qualification allocation. Set `cpus_per_task = 8`; obtain partition, memory,
walltime, and concurrency from the Nibi template and a measured pilot. Do not
pack two ANTs registrations into one allocation until peak memory proves that
safe and useful.

The stage scripts read `RS_PARAMS_JSON` and `RS_OUTPUTS_JSON`, supplied by the
campaign wrapper. They must not derive writable paths from unvalidated shell
arguments. Every declared output must live below the campaign `output_root`.

Campaign output contracts require:

- `cardinality = "exactly_one"` and `min_bytes = 1` for every artifact;
- settling across existence, size, mtime, and checksum before validation;
- a command validator that checks H5 readability, direction metadata, and the
  expected provenance/QA schema;
- SHA-256 in the campaign receipt when each H5 is below remoteslurm's 256 MiB
  per-file hashing cap (the current TemplateFlow files are about 205 MB);
- otherwise, a job-produced SHA-256 sidecar plus a command validator, followed
  by an unconditional local re-hash after transfer;
- preservation of scheduler logs, attempt IDs, output evidence, and failed
  validation receipts.

The intended lifecycle is:

```text
rslurm campaign plan campaign.nibi.toml
rslurm campaign preflight campaign.nibi.toml --against v1
rslurm campaign start campaign.nibi.toml --run-id transform-artifacts-v1
rslurm campaign apply neuroatlas-transforms --run transform-artifacts-v1 \
  --require-preflight v1 --max-groups 1
rslurm campaign status neuroatlas-transforms \
  --run transform-artifacts-v1 --refresh
rslurm campaign verify neuroatlas-transforms \
  --run transform-artifacts-v1 --stage build
rslurm campaign apply neuroatlas-transforms \
  --run transform-artifacts-v1 --require-preflight v1 --max-groups 1
rslurm campaign verify neuroatlas-transforms \
  --run transform-artifacts-v1 --stage qualify
rslurm campaign close neuroatlas-transforms --run transform-artifacts-v1
```

Use the actual campaign name emitted by `plan`; the example assumes
`name = "neuroatlas-transforms"`. Monitor with bounded `status`/`jobs` calls and
use `diagnose` for failed or unexpectedly pending allocations.

Never blindly resubmit after a lost response. A unit in `UNKNOWN` stays unknown
until reconciled; accepting duplicate execution requires an explicit retry with
the recorded reason and `--accept-duplicate-risk`. Validation failures also
require an explicit selector and reason. Prior attempts and receipts remain
part of the evidence.

After the `qualify` receipt passes, pull only the qualified candidate directory
and compact evidence through the configured results project or a bounded
`rslurm get --rsync`. Recompute every SHA-256 locally and compare it with the
remote receipt. Keep GitHub credentials and release publication on the local
machine; Nibi never needs a GitHub token.

### Remoteslurm pain-point and bug protocol

Record every operational interruption in the campaign work log with the exact
step, command, `remoteslurm` revision, definition/run/attempt identity, and
whether the scheduler accepted work. Classify it before filing:

- **site/configuration**: MFA expiry, missing Nibi host/project configuration,
  account or partition policy, unavailable module/container, quota, or
  permissions;
- **workflow/specification**: invalid campaign schema, wrong output contract,
  insufficient requested resources, or an incorrect validator;
- **remoteslurm product defect**: crash, unsafe or ambiguous resubmission,
  incorrect campaign state, lost/contradictory attempt identity, invalid wrapper
  rendering, output/settling/hash error, broken recovery, or behavior that
  contradicts the documented contract.

Do not file the first two categories as remoteslurm bugs. For a suspected
product defect:

1. Preserve the original run, attempt, logs, JSON response, and receipts. Do not
   mutate evidence or retry blindly.
2. Reproduce with the smallest safe campaign, preferably the synthetic Nibi
   canary or the local FakeSlurm/testkit. Never use the 205 MB scientific files
   when a tiny fixture demonstrates the same defect.
3. Search existing issues in `bbuchsbaum/remoteslurm` to avoid duplication.
4. File a GitHub issue at `https://github.com/bbuchsbaum/remoteslurm/issues`
   containing expected versus actual behavior, minimal reproduction, exact
   revision, sanitized structured error, scheduler acceptance state, and impact
   on campaign safety or usability.
5. Remove credentials, user-specific absolute paths, account names, allocation
   IDs that should remain private, and scientific data from the report.
6. Link the issue number in the transform campaign work log. Continue only with
   a workaround that preserves attempt identity, duplicate-submission safety,
   output contracts, and validation evidence; otherwise stop the campaign.

Feature requests and repeated usability friction may also be filed, but label
them as such and include the concrete Nibi workflow that made the need visible.
The absence of an MFA connection while this plan was written is expected setup
state, not a remoteslurm bug, so no issue is warranted yet.

Rename qualified candidates without altering their bytes:

```text
tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
```

## 7. Qualification contract

### 7.1 Freeze the policy first

`qualification-policy.json` contains all numeric thresholds and the precise
images, masks, labels, landmarks, grids, and interpolation modes used for
qualification. Establish thresholds from the official TemplateFlow transform,
the identity/no-warp baseline, and a pilot build. Freeze and review the policy
before evaluating the release candidate; do not relax a threshold after seeing
a candidate failure.

The identity baseline is necessary because inverse consistency alone can be
excellent for a transform that performs no useful anatomical mapping.

### 7.2 Mechanical and provenance gates

- Every declared file exists and is non-empty.
- SHA-256 and byte size match the assembled manifest.
- The H5 structure is readable by ANTs and by
  `neurotransform::read_transform(type = "ants_h5")`.
- The embedded displacement grid and affine component have finite, plausible
  geometry.
- Source and target directions match the manifest. A transform that only works
  after silently swapping source and target fails.
- The inverse asset is associated with the same build attempt and input pair.
- The pinned container, command, preset, inputs, logs, and package revisions are
  present in provenance.

### 7.3 Registration and deformation gates

Run `niflowr::ni_ants_registration_qa()` in each direction and record:

- dense MI and CC costs in the fixed mask;
- brain-mask Dice;
- per-label Dice for a frozen set of independently selected anatomical labels;
- Jacobian determinant summaries;
- the same measurements before registration.

Required invariant gates:

- all reported metrics are finite;
- zero non-finite Jacobians in the evaluation mask;
- zero non-positive Jacobians in the evaluation mask;
- warped image similarity improves over the identity baseline;
- warped mask overlap improves over the identity baseline;
- label IDs remain integers and no unexpected IDs appear;
- the qualified target image has exactly the requested target geometry.

### 7.4 Independent direction and numerical evidence

Do not rely only on ANTs applying a transform that ANTs produced.

1. Freeze a stratified set of physical-space landmarks covering central,
   cortical, subcortical, boundary, and near-edge regions.
2. Transform them with ANTs tooling and independently through
   `neurotransform`; compare physical RAS coordinates.
3. Apply forward then inverse to the landmark set and report median, p95, and
   maximum return error. This is an inverse-consistency check, not evidence of
   anatomical accuracy by itself.
4. Compare the neuroatlas candidate with the official TemplateFlow route on the
   same landmarks, masks, continuous image, and label images. Record differences
   rather than requiring byte or field identity.
5. Independently inspect output NIfTI dimensions, affine, finite values, label
   set, and probability range using RNifti or another reader outside the apply
   path.
6. Repeat the same pinned build once. Byte identity is not required from
   multithreaded ANTs, but the resulting landmark mappings and frozen QA metrics
   must agree within policy tolerances.

### 7.5 Data-type application gates

- Continuous scalar image: linear interpolation; finite output and improved
  similarity.
- Integer atlas: `GenericLabel` or nearest-neighbour interpolation; integer
  output, no novel labels, and explicit reporting of any lost label.
- Probabilistic atlas: linear interpolation, finite values, frozen range
  tolerance, and an explicit renormalisation policy if channels are expected to
  sum to one.
- Route composition: compare a composed transform with sequential point
  transforms, then verify that image application performs a single resampling.
- Round trips are reported for masks, landmarks, and representative labels, but
  are never the sole release criterion.

All failures stay in the evidence bundle. A failed route remains `candidate` or
`planned`; it is never published as `available` by dropping a failing case.

## 8. Release bundle and GitHub distribution

Create a dedicated GitHub Release attached to the `neuroatlas` repository:

- tag: `transform-artifacts-v1`
- title: `neuroatlas transform artifacts v1`
- purpose: binary data distribution, independent of the R package version

Release assets:

```text
tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5
tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5
transform-artifacts-v1.json
transform-artifacts-v1-qa.json
transform-artifacts-v1-provenance.json
transform-artifacts-v1-report.html
SHA256SUMS
LICENSES.md
```

The manifest contains one row/object per directed artifact with:

- `artifact_id` and `pair_id`;
- `artifact_version`;
- `from_space` and `to_space`;
- representation and transform type;
- provider (`neuroatlas` or `templateflow`);
- filename and immutable release URL;
- SHA-256 and byte size;
- H5 format and ANTs direction convention;
- source and target template queries and input digests;
- target grids on which it was qualified;
- recipe, `niflowr`, ANTs, and container revisions;
- qualification-policy digest and summary status;
- license and citation information.

Publication sequence:

1. Assemble a release directory from qualified attempt outputs only.
2. Verify the assembled directory from scratch against `SHA256SUMS`.
3. Complete the template and derivative redistribution/license review.
4. Create a draft release and upload all assets.
5. Download the draft assets through the GitHub API and recheck size, SHA-256,
   H5 readability, and manifest consistency.
6. Publish the release.
7. Download both H5 files anonymously from their final URLs into an empty cache
   and repeat the checks plus one apply smoke test per direction.
8. Only then commit the `neuroatlas` registry change from `planned` to
   `available`.

Do not use Git LFS. Do not include release binaries in GitHub-generated source
archives, the R source tarball, `inst/extdata`, tests, or pkgdown.

If a released artifact is wrong, mark it retired in the next package registry
and issue `transform-artifacts-v2`. Do not overwrite V1 assets.

## 9. Neuroatlas registry

Keep `inst/extdata/transform_registry.csv` as the small package-shipped index,
but extend it with stable artifact fields:

```text
artifact_id
artifact_version
provider
url
sha256
size_bytes
format
qualification
qa_url
license
```

Each directed H5 receives its own row. Both rows share a `pair_id` in the
detailed release manifest. The package registry is authoritative for routing;
the release manifest is authoritative for the artifact's detailed provenance.

Statuses have strict meanings:

- `planned`: route design only; no usable artifact is promised;
- `candidate`: artifact exists but has not passed all release gates;
- `available`: final URL, checksum, license, load, apply, and qualification gates
  passed;
- `retired`: retained for provenance but never selected automatically.

Route selection ignores `candidate`, `planned`, and `retired` unless a caller
explicitly requests diagnostic planning.

## 10. Neuroatlas public API

### `space_transform_manifest()`

Extend the existing function rather than creating a competing manifest API. It
returns routing plus provider, artifact version, size, checksum, qualification,
and availability metadata. It performs no downloads.

### `atlas_transform_plan()`

Upgrade the current direct/two-hop search to a general weighted graph search.
Default ordering is:

1. `available` routes only;
2. qualified direct route before composed route;
3. stronger qualification/confidence;
4. fewer transform edges;
5. deterministic artifact-ID tie-break.

The returned plan includes exact artifact IDs, directions, expected downloads,
total bytes, and any interpolation or uncertainty warnings. Planning remains
side-effect free.

### `get_template_transform()`

Proposed signature:

```r
get_template_transform(
  from,
  to,
  provider = c("auto", "neuroatlas", "templateflow"),
  download = TRUE,
  verify = TRUE,
  cache_dir = transform_cache_path(),
  offline = FALSE
)
```

It resolves a plan, downloads missing files, verifies byte size and SHA-256,
loads each H5 with `neurotransform`, and returns a `template_transform` object
containing the route, local files, and manifest/provenance records.

Download behavior:

- cache generated assets under
  `tools::R_user_dir("neuroatlas", "cache")/transforms/<artifact-version>/`;
- continue using TemplateFlow's own cache for official TemplateFlow assets;
- download to a unique temporary partial file in the destination filesystem;
- verify size and SHA-256 before an atomic rename;
- use a per-artifact lock so parallel callers do not duplicate a 205 MB
  download;
- delete a failed partial file, never a previously verified cache entry;
- in offline mode, return only verified cached artifacts or fail with the exact
  missing artifact IDs;
- never resolve a mutable `latest` release URL.

Add `transform_cache_path()` and `clear_transform_cache()` for this cache only.
Do not make clearing transform artifacts implicitly wipe the TemplateFlow cache.

### `apply_template_transform()`

Proposed signature:

```r
apply_template_transform(
  x,
  transform,
  target,
  data_type = c("auto", "continuous", "label", "probability"),
  interpolation = NULL
)
```

It uses `neurotransform` to compose the route and resample exactly once. Default
interpolation is linear for continuous/probability data and `GenericLabel` or
nearest for labels. It rejects ambiguous data types instead of guessing from
integer-valued samples alone.

The result records source/target identity, artifact IDs and SHA-256 values,
target geometry, interpolation, package versions, and operation history.

### `map_atlas()`

Provide an atlas-aware wrapper:

```r
map_atlas(atlas, to_space, resolution = NULL, provider = "auto", ...)
```

It:

- obtains an explicit target template grid;
- selects label or probability interpolation from atlas representation;
- preserves semantic parcel IDs, labels, colours, citations, and parent
  metadata;
- records lost/empty labels rather than silently deleting or renumbering them;
- validates that no novel label IDs were created;
- appends transform artifact and target-template provenance;
- constructs a valid `ClusteredNeuroVol` only after the ID mapping is explicit.

`resample()` must remain a same-template grid operation. It must not silently
start performing nonlinear cross-template warps.

## 11. Package dependencies and cache correction

- Add `neurotransform` to `Suggests` with `requireNamespace()` guards around
  apply functionality.
- Keep `niflowr` out of runtime dependencies; document it as a development/build
  dependency under `data-raw`.
- Choose one SHA-256 implementation and declare it explicitly rather than
  shelling out to platform-specific commands.
- Use base download support unless retries, redirects, or progress behavior
  justify a small declared HTTP dependency.
- Fix `show_templateflow_cache_path()`: the installed pure-R `templateflow`
  package exports `tf_client()` but not `tf_home()`. Read the public client's
  cache root with a guarded fallback, and keep the generated-transform cache
  separate.

## 12. Tests and qualification cadence

### Ordinary package CI

- Registry schema, status semantics, normalization, and graph routing.
- Direct, inverse, multi-hop, retired, missing, and tie-break routes.
- Mocked download, retry, lock, checksum mismatch, partial-file, offline, and
  atomic-cache behavior.
- Synthetic small ANTs H5 fixtures for load/direction/application contracts.
- Continuous, label, and probability interpolation behavior on analytic toy
  volumes.
- Provenance preservation and rejection of ambiguous or inconsistent metadata.
- No network and no 205 MB files.

### Scheduled/manual integration

- Anonymous download from the published GitHub Release.
- SHA-256 and size verification.
- Actual H5 load through `neurotransform`.
- One scalar and one label application in each direction.
- Exact target geometry and label-set checks.

### Artifact-release qualification

- Full input and output provenance.
- All Section 7 gates on 1 mm and 2 mm targets.
- Official TemplateFlow comparison.
- Independent landmark and image-reader checks.
- Repeated build stability.
- Rendered QA report reviewed before release publication.

Package CI proves software behavior. Artifact-release qualification proves the
specific large binaries. Neither substitutes for the other.

## 13. Work packages and stop gates

### WP0: freeze V1 contract

- Confirm V1 pair and exact TemplateFlow input queries.
- Resolve redistribution licenses.
- Capture input bytes and digests.
- Write and review `routes.json` and the qualification policy.
- Connect/configure Nibi, capture `rslurm info`, and freeze named project,
  environment template, storage roots, and site resource policy.

Stop if template identity, license, or direction cannot be made explicit.

### WP1: Nibi campaign canary

- Compile, preflight, start, run, verify, transfer, and close the isolated
  synthetic smoke campaign.
- Record friction separately from defects and lodge validated remoteslurm issues
  using the protocol above.
- Preserve definition, run, attempt, and receipt identities.

Stop if campaign state is ambiguous, a submission cannot be reconciled, output
validation is contradictory, or a safety-relevant defect lacks a disposition.

### WP2: reproducible builder

- Add build scripts and ignored work layout.
- Pin the ANTs container and `niflowr` revision.
- Compile and preflight the production Nibi campaign.
- Generate one forward/inverse candidate pair through managed campaign
  execution.
- Preserve complete attempt evidence.

Stop if either declared composite is absent or provenance is incomplete.

### WP3: qualification

- Run niflowr QA, independent landmark checks, datatype checks, grid checks,
  TemplateFlow comparison, and repeated-build comparison.
- Render the report and retain every failed case.

Stop if any frozen required gate fails. Do not publish an artifact with a
qualified direction inferred only from filenames.

### WP4: artifact release

- Assemble assets, manifests, checksums, report, and licenses.
- Upload a draft, redownload, verify, publish, and anonymously requalify.

Stop if the live bytes differ from the qualified candidate.

### WP5: neuroatlas resolver and cache

- Extend the registry and route planner.
- Implement verified atomic download/cache behavior.
- Implement `get_template_transform()` and cache management.
- Fix TemplateFlow cache-path discovery.

Stop before marking routes available if live anonymous download is not proven.

### WP6: application API

- Add optional `neurotransform` integration.
- Implement single-resampling composition, typed interpolation, and `map_atlas()`.
- Preserve provenance and semantic IDs.

Stop if direction, data type, or target geometry is ambiguous.

### WP7: documentation and release

- Add a template-mapping vignette with cached/small examples.
- Document download size, cache location, offline use, licenses, and how to clear
  only the transform cache.
- Run focused tests, the full package tests, package check, and the live artifact
  integration job.

Do not claim general TemplateFlow all-pairs support in V1.

## 14. V1 definition of done

V1 is complete only when:

- one pinned `niflowr` build has produced the forward/inverse pair;
- the remoteslurm Nibi canary has a reconciled terminal state and immutable
  receipts, with validated product defects filed or explicitly absent;
- both directions pass the frozen qualification policy;
- `transform-artifacts-v1` is publicly downloadable and anonymously verified;
- final asset SHA-256 values and sizes are in the package registry;
- `atlas_transform_plan()` returns executable available routes in both
  directions;
- `get_template_transform()` verifies and caches both artifacts;
- `apply_template_transform()` and `map_atlas()` pass real-H5 scalar and label
  integration tests;
- the package tarball and Git history contain no large warp binaries;
- documentation states the exact supported spaces, grids, interpolation rules,
  cache behavior, provenance, and limitations.

## 15. Immediate next action

Implement WP0 only: resolve the exact TemplateFlow source/target T1w and mask
files, record their revisions and SHA-256 values, check their licenses, and
write the frozen route plus qualification-policy drafts. In parallel, establish
the user-authenticated Nibi connection and capture `rslurm info` so the campaign
uses real site policy rather than guessed resources. Do not start the synthetic
canary or costly registration until those inputs, gates, and remote settings are
reviewable.

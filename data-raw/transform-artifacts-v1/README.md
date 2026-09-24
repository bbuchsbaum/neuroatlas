# Transform artifacts V1 build surface

This directory builds and qualifies the two directions of the nonlinear V1
template transform. It is source and policy only: generated inputs, attempts,
and release staging live in ignored `data-raw/transform-artifacts-v1-work/`.
The large H5 files are GitHub Release assets named `transform-artifacts-v1`,
never Git or package payloads.

## Current state

`routes.json` freezes six TemplateFlow files: source/target 1 mm brain T1w,
brain masks, and matching HOCPA label maps. Each has its original relative
path, byte count, and SHA-256. `materialize-inputs.R` re-fetches them through
TemplateFlow, verifies the receipts, and makes byte-preserving local copies for
remote synchronization.

The registration direction is moving `MNI152NLin6Asym` to fixed
`MNI152NLin2009cAsym`. One `niflowr::ni_ants_register_to_template()` run emits
the ANTs composite H5 and inverse composite H5; those files are only renamed
after their bytes are copied and hashed.

## Safe local preparation

```r
Rscript data-raw/transform-artifacts-v1/materialize-inputs.R
```

The script does not execute ANTs. It stops on any changed TemplateFlow input
instead of replacing a frozen receipt.

## Nibi campaign gate

`campaign.nibi.toml` and `campaign.nibi-smoke.toml` are templates, not
submittable definitions. Their roots are literal `__NIBI_*__` sentinels. Before
replacing them, obtain Nibi's authenticated facts:

```text
remoteslurm connect nibi
rslurm --json -H nibi info
```

Use those results to configure a bounded Nibi host/template and a separate
non-deleting sync project for the scripts/frozen inputs and for results. The
remoteslurm host must be a named `nibi` profile with an explicit reusable
`ControlPath`; a bare `user@host` connect is not a substitute. Create
`niflowr.nibi.yml` from the template, generate and validate its lockfile on
Nibi, and use the development GitHub niflowr reference recorded in
`routes.json`. Every smoke/build provenance record captures its resolved
immutable `RemoteSha`; a moving `main` reference is never sufficient for a
release. Mirror the generated `transform-artifacts-v1-work/inputs/`
directory to the same relative path under the remote checkout (or set
`NEUROATLAS_TRANSFORM_WORK_ROOT` to its remote parent). Then run the smoke
campaign before the production one. The smoke creates a tiny shifted synthetic
NIfTI pair and runs niflowr's `testing` ANTs preset, producing composite and
inverse H5 files that its validator independently opens with neurotransform.
Those files remain explicitly non-production evidence. The campaign
scripts consume only `RS_PARAMS_JSON` and `RS_OUTPUTS_JSON` supplied by
remoteslurm; they do not accept writable locations as shell arguments.

`campaign.nibi.toml` has a `build` array stage and a `qualify` array stage that
requires a **verified** build. Every published candidate output has an
existence, cardinality, byte-size, settling, and validator contract. H5 hashes
are requested while each H5 remains below remoteslurm's 256 MiB per-file cap;
release assembly always re-hashes them locally after transfer.

## Qualification and distribution gate

`qualification-policy.json` now freezes the identity baselines and prospective
engineering thresholds after independent review. Its approval concerns the
protocol only. Candidate and repeat must independently improve MI, CC and mask
Dice over identity on all four direction/grid cells, satisfy numerical and
deformation gates, and agree within the frozen repeat tolerances. The official
TemplateFlow pair failed calibration; it remains diagnostic evidence.

`landmarks-v1/` contains the reviewed coordinate files, input receipts and policy
review. The five disjoint numerical strata have 32 points per hemisphere each.
These are numerical coverage probes, not paired anatomical truth. Regenerating
them reproduced identical CSV bytes. `materialize-landmarks.R` refuses to replace
an existing frozen output directory.

The build, calibration and qualification campaign templates are separate.
Current run IDs, scheduler receipts and retained failures are in
`campaign-work-log.md`. Qualification outputs alone do not confer release
eligibility: the visual and numerical landmark reviews must bind exact evidence.

Qualification also renders a compact visual dashboard with `neuroim2`: native
source/target context, target-versus-warped checkerboard, brain-mask and HOCPA
label-boundary overlays, and Jacobian-minus-one overlay. It is a required
review artifact and Release asset, but never a substitute for the numerical
gates. The dashboard is self-contained HTML plus PNG files and a checksummed
manifest, so it can be reviewed without a live R session.

Once the policy passes, release assembly produces two H5 assets,
`transform-artifacts-v1.json`, compact QA/provenance files, and `SHA256SUMS`.
Publish those as immutable GitHub Release assets. Only then may the packaged
space registry change these routes from `planned` to `available`; the runtime
resolver will download into the user cache and verify the published byte count
and SHA-256 checksum before exposing an artifact to `neurotransform`.

## Public-asset integration

`scripts/verify-live-release.R` exercises the installed candidate package with
an empty dedicated transform cache. It downloads both directions through
`get_template_transform()`, applies a scalar and label image on each 2 mm grid,
compares against the separately executed native ANTs qualification outputs, and
reopens the artifacts offline to check identical label application.

The four arguments are the local work root, an empty cache directory, a new
JSON report path, and the assembled release-evidence directory. The latter must
contain its reviewed `qa.json` and `raw/{forward,inverse}_2mm/` files
`scalar-linear.nii.gz`, `labels-nearest.nii.gz`, and
`label-oracle-allowed-differences.nii.gz`. Inputs, candidates and native references
are bound to qualified receipts; the reviewed QA is bound to the manifest
fetched from the public release. Label disagreements are allowed only at the
frozen tie/edge locations. The check must pass after anonymous publication and before
committing available registry rows. Large artifacts stay out of package tests.

## Remoteslurm issue protocol

MFA, quotas, scheduler policy, site modules, and a malformed campaign are not
remoteslurm bugs. For a suspected product defect, preserve the original
campaign/run/attempt ids and receipts; reproduce it with the smoke fixture or
FakeSlurm; search existing issues; then file a sanitized report at
<https://github.com/bbuchsbaum/remoteslurm/issues>. Never retry an `UNKNOWN`
attempt without the explicit duplicate-risk authorization and recorded reason.

# fsaverage density route qualification

This campaign adds exact directed native routes between pinned fsaverage 164k
and fsaverage6 41k / fsaverage5 10k, separately for L and R. It uses the existing
`native_closest_barycentric` engine at revision
`933edddda462593941e167726e8aaa7168ff103a`. Each direction is evaluated separately;
upsampling is not an inverse. No new runtime dependency is introduced.

## Inputs and prospective gates

`contract-v1.json` was frozen before evaluating candidate operators. Weight,
relative projection and bounded-value errors must stay within **1e-12**. The
protocol fixes seed 20261007, 256 random targets plus 64 targets with small
positive weights per route (duplicates removed), and 14 analytical geometries.
All eight routes must pass. A failure is retained rather than omitted or admitted
under a relaxed threshold.

Released `surface-inputs-v1.json` and `surface-domains-v1.json` remain unchanged.
The new installed `surface-density-inputs-v1.json` locks original upstream bytes
and exact archive members; `surface-density-domains-v1.json` binds the four new
ordered domains. Sources are:

- [TemplateFlow fsaverage at the pinned revision](https://github.com/templateflow/tpl-fsaverage/tree/8e53ba4f2e438758f69d11436fe0cd291a28bec6): 10k/41k L/R spheres, verified against its Git-annex MD5 keys and then SHA-256 locked.
- [Pinned neuromaps OSF manifest](https://github.com/netneurolab/neuromaps/blob/ffcc2e0f657943ce00a1b6a968396f32250e495c/neuromaps/datasets/data/osf.json): cortical masks, vertex areas and ordered sphere references, with both archives MD5-verified and SHA-256 locked.

`input-ordering-receipt.json` records exact equality of ordered sphere coordinates
and triangles across providers. Every lower-density coordinate also exactly
matches its corresponding prefix of the original 164k sphere. The masks are
binary and areas finite and positive. Vertex areas identify domains; ordinary
barycentric resampling does not use them for area conservation. Raw spheres,
masks and areas stay in the local work directory, downloaded from their original
providers. The installed license addendum retains the existing upstream notices
and download-only distribution decision.

## Independent reference arithmetic and retained failure

The first exhaustive float64 reference run failed on 10k L -> 164k L, target
72203 (zero based). It selected the common triangle edge, while the native
operator retained a positive third contributor of 2.255372555e-9. The resulting
weight discrepancy exceeded the frozen 1e-12 gate. The original script, output
and completed progress records remain in `evidence/float64-failure/`.

An independent 80-digit least-squares calculation showed that the native
interior projection was closer: squared distance
0.000210590542647321545995755 versus
0.000210590542647383925580718 for the edge, a difference of about 6.24e-17.
Float64 distance arithmetic reversed that ordering. This was a reference
arithmetic failure; the engine and its weights were not changed.

`oracle-refinement-v1.json` freezes a deterministic precision refinement before
rerunning every query. The oracle evaluates every triangle interior and every
closed segment, independently of the production tree/search. All faces with a
primitive inside a conservative floating-point roundoff envelope of the global
minimum are re-evaluated at both 80 and 100 decimal digits. Triangle condition
numbers must be at most 10; precision responses must agree within 1e-20. Exact
query/vertex equality uses its exact unit basis response. The original **1e-12
acceptance tolerances remain unchanged**. No failure-specific whitelist, target
omission or native zero-weight repair is used.

## Checks and scope

The evidence binds exact input identities, ordered exported arrays, scripts,
contract, engine build and candidate operators. It covers:

- Exhaustive independent geometry checks for all frozen sampled targets,
  including near-boundary support and the analytical impulse responses.
- Every target row: finite positive coefficients, unit mass, constant and convex
  range preservation, face order/winding invariance, source exclusion and target
  masking. Downsampling must select exactly the matching source vertex, with
  zero contribution from other vertices.
- Every full-density output: independent continuous accumulation, availability,
  aggregate-label voting and largest-weight-label selection, including supported
  key zero. Probability channels retain partial mass and label tables persist.
- Synthetic exact ties, all-excluded support, tiny positive contributors and
  zero-weight missingness; real-density all-source, supported-zero and
  propagate/omit/error policies.
- Admitted public `get_template_transform()` / `apply_template_transform()`
  resolution, identical candidate coefficients and values, and verified offline
  operator and geometry replay.

Numerical qualification covers these exact inputs and policies. It does not
establish anatomical accuracy, Workbench equivalence, adaptive/area-conserving
resampling, parcel preservation or reversibility. Lower-density labels can lose
small parcels; inspect key counts and coverage for the actual data. The existing
fine-parcel projection, independent anatomy/expert review and atlas-version
Motes remain separate work.

## Results

All eight directed routes pass the unchanged gates. The fresh geometry reference
covers 2,555 real-mesh targets and 84 analytical queries. Maximum weight error
is 1.48e-14; maximum relative point error is 3.26e-16. Every full-density
continuous/label array has zero label or availability disagreements; maximum
independent continuous accumulation error is 2.22e-16.

| Hemisphere | Direction | Geometry queries | Available / target vertices | Maximum weight error |
| --- | --- | ---: | ---: | ---: |
| L | 164k -> 41k | 319 | 37461 / 40962 | 0 |
| L | 41k -> 164k | 320 | 149904 / 163842 | 1.4e-14 |
| L | 164k -> 10k | 318 | 9353 / 10242 | 0 |
| L | 10k -> 164k | 320 | 149852 / 163842 | 4.55e-15 |
| R | 164k -> 41k | 318 | 37452 / 40962 | 0 |
| R | 41k -> 164k | 320 | 149879 / 163842 | 1.48e-14 |
| R | 164k -> 10k | 320 | 9354 / 10242 | 0 |
| R | 10k -> 164k | 320 | 149872 / 163842 | 5.88e-15 |

Unavailable target vertices include the target medial wall and any target
with no included source contributor. Original source/target masks are retained;
no nearest valid vertex is substituted. The case manifests bind every exported
binary array by SHA-256, while the published receipts contain no raw meshes.
The initial public coefficient comparison also included engine benchmark
timings, which differ across builds. Its failed log is retained. The final
comparison requires exact equality of every plan field except execution time,
plus exact output/coverage equality and identical offline cache replay.

## Package validation

R 4.3.3, neuroim2 0.19.1 and the pinned neurotransform 0.2.0 build:
`devtools::document()` and the project lint gate pass. The full test suite has
3,089 passing assertions, 44 existing warnings and two opt-in integration skips
(HCPEX network and TemplateFlow). `devtools::check(manual = TRUE)` with tests
run separately passes with zero errors, zero warnings and three environment/size
notes: installed package size, unavailable current-time verification and missing
HTML validator `tidy`. Vignettes and examples, including donttest examples, build
and run successfully. The new focused density tests pass 127 assertions.

## Reproduction

Run from the repository root with the pinned neurotransform build in `R_LIBS`
and a complete `NEUROATLAS_ENGINE_BINDING` receipt. Set
`NEUROATLAS_SURFACE_INPUTS` to the original verified 164k input directory.
Qualification uses Python NumPy 1.26.4 and mpmath 1.3.0, and the existing optional
R GIFTI/jsonlite packages. These are campaign tools, not new runtime dependencies.
Use a fresh output folder and a dedicated transform cache:

```sh
python prepare-inputs.py /local/density-inputs
Rscript prepare-domains.R /local/density-inputs
Rscript qualify.R /local/density-candidate /local/transform-cache
python qualify-oracle-precise.py /local/density-candidate
python check-full-arrays.py /local/density-candidate
Rscript check-policies.R /local/density-candidate
python register-routes.py /local/density-candidate
Rscript check-public-routes.R /local/density-candidate /local/transform-cache
```

Prefix script filenames above with `data-raw/fsaverage-density-v1/`. Route
registration refuses admission unless every gate receipt passes and its contract,
consumer and input hashes match. The registration script admits new rows or verifies identical existing rows;
it refuses to replace different existing density identities. Rebuilding
the input manifests or changing upstream bytes creates different domain
identities and requires new qualification; existing identities are not reused.

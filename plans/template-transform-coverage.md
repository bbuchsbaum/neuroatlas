# Template transform coverage: surfaces and projections

Assessment date: 2026-09-25.
Implementation tracker: `bd-01M3CCBG9346NRV3WEVH5YNBCQ`.
Updated: 2026-10-06. The explicit-domain native surface slice is implemented;
generic named-space routes and the broader release remain unfinished.

## Outcome

Make neuroatlas the public entry point for discovering, fetching, planning, and
applying qualified template mappings across volumes and cortical surfaces. Reuse
established correspondence assets and numerical engines. Keep registration fitting
in preprocessing. Extend the existing registry, cache, provenance, and public API
rather than creating a separate surface registry.

The completed [V1 volume release](transform-artifacts-v1.md) remains unchanged.
Subject-specific registration, reconstruction, and unrestricted all-pairs fitting
are outside this expansion. Importing an existing subject registration can be a
later integration; a population mapping cannot substitute for it.

## Verified starting point

| Capability | Current neuroatlas status |
| --- | --- |
| MNI152NLin6Asym <-> MNI152NLin2009cAsym | Qualified volume application at 1 and 2 mm, immutable artifacts |
| MNI305 <-> generic MNI152 | Legacy coordinate affine; exact template identity and anatomical scope need audit |
| fsaverage <-> fsaverage5/6 | Corrected to planned, approximate, directed; no named-space executor |
| fsaverage <-> fsLR 32k | Planned registry edges |
| Exact fsaverage 164k <-> registered fsLR 32k, L/R | Explicit-domain native API implemented; historical scoped numerical gates pass; no Workbench equivalence claim |
| MNI volume -> fsaverage | Planned transform edge; separate visualization sampling exists |
| Surface -> volume | Planned registry edge |
| CIVET, CIFTI mapping, other MNI variants, cerebellar and age-specific routes | Not covered by the qualified V1 service |

Evidence anchors:

- `inst/extdata/transform_registry.csv`: current route declarations.
- `R/template_transform.R`: resolver accepts image transforms; application rejects
  surface atlases.
- `R/coordinate_spaces.R`: legacy generic MNI152 coordinate classifications and
  affine vertex-coordinate conversion, not cortical correspondence.
- `R/ce_overlay.R` and `R/plot_brain.R`: white/pial sampling for visualization;
  callers must already supply aligned volume and surface coordinates.
- `R/alignment_registry.R`: volume/surface atlas relations remain planned.
- Sibling `neurotransform/R/surface_resampling.R`, `sampler.R`, and `morphism.R`
  provide resampling/projection primitives. Their existence does not qualify a
  named template route. A matrix transpose or adjoint is not an inverse mapping.

## Mapping methods

### Surface to surface

Use source and target registration spheres in a shared correspondence frame.
Nearest points on inflated or anatomical surfaces are not a substitute for that
registration. A mesh family, hemisphere, density, vertex ordering, topology, and
registration revision are part of the domain identity; vertex count alone is
insufficient. White, pial, midthickness, and inflated geometries can share a vertex
domain without requiring resampling between their data arrays.

Start with fsaverage 10k/41k/164k and fsLR 32k/59k/164k, qualifying each direction
and density actually advertised. TemplateFlow already distributes fsLR spheres
in fsaverage correspondence, medial-wall masks, and average vertex-area metrics.
These are assets to pin and qualify, not registrations to refit by default.
[TemplateFlow fsLR inventory](https://github.com/templateflow/tpl-fsLR).

Use Workbench as the initial independent reference: area-aware metric resampling
for ordinary continuous maps and a categorical label operator for parcellations.
Labels must never be numerically averaged. Barycentric geometry resampling is a
different operation from resampling values on a mesh. Treat both directions as
available operators, not a promise of lossless round-trip recovery.
[Metric resampling](https://files.humanconnectome.org/software/workbench-command/-metric-resample),
[label resampling](https://files.humanconnectome.org/software/workbench-command/-label-resample).

### Volume to surface

Expose two distinct methods with explicit provenance:

1. **Registration fusion for population template maps.** Use released mappings
   that associate target cortical vertices with sampling coordinates in a
   specific volume template. CBIG and neuromaps provide established precedents.
   Verify the exact upstream volume bytes, variant, and affine before connecting
   an asset named only MNI152 to our graph. Never infer equivalence with 2009c or
   another MNI variant from that shorthand. Apply a qualified volume warp first
   when required; where valid, compose its pullback with sampling coordinates to
   avoid an intermediate volume resampling.
2. **Explicit anatomical ribbon sampling.** Given registered white/pial surfaces
   and a volume in the same physical frame, sample the cortical ribbon using a
   declared method. Subject-specific surfaces require their actual registration.
   Population average ribbon sampling must not be presented as registration
   fusion. Inflated coordinates are display geometry, not sampling coordinates.

Both operations discard information outside their sampled cortical support.
Return coverage/missingness and preserve hemisphere and medial-wall masks.
Provide distinct scalar, categorical, and probability semantics; never silently
renormalize probability channels or convert unsupported vertices into valid zero.
[CBIG registration fusion](https://github.com/ThomasYeoLab/CBIG/tree/master/stable_projects/registration/Wu2017_RegistrationFusion),
[neuromaps transformations](https://netneurolab.github.io/neuromaps/user_guide/transformations.html),
[Workbench ribbon mapping](https://dp.humanconnectome.org/software/workbench-command/-volume-to-surface-mapping).

### Surface to volume and mixed representations

Rasterization into a specified ribbon/grid or a separately supplied registration
fusion map is a distinct directed operation. It cannot recover discarded volume
data and must not be synthesized by inverting volume-to-surface projection.
Preserve unmapped voxels and label conflicts explicitly.
[Workbench label-to-volume mapping](https://www.humanconnectome.org/software/workbench-command/-label-to-volume-mapping).

CIFTI is a container for cortical surface domains and volumetric structures, not
another coordinate system. Map cortex through surface correspondence, subcortex
through the appropriate volume route, and preserve brain-model indices, masks,
structure identities, and label tables. A cortical projection must not silently
drop subcortical structures.
[Workbench CIFTI resampling](https://www.humanconnectome.org/software/workbench-command/-cifti-resample).

## Ordered implementation scope

1. **Correct the capability contract.** Separate catalogued assets, executable
   backends, and qualified routes. Audit legacy available/exact/reversible surface
   entries and generic MNI152 claims with compatibility tests. Resolve domains by
   exact template/mesh identities, not coordinate-family names. Make planning
   aware of volume, surface, and mixed representations and the requested data
   semantics. Disallow lossy surface projection as an automatic volume-to-volume
   shortcut. A reverse artifact is separate from mathematical invertibility.
2. **Qualify cortical surface routes.** Integrate the existing surface operators
   behind the public planner/apply workflow. Deliver fsaverage density changes,
   fsLR density changes, and fsaverage <-> fsLR. Begin with fsaverage 164k <-> fsLR
   32k, then expand the tested density matrix. Pin spheres, area metrics, masks,
   topology/ordering hashes, and algorithm revisions in verified cache artifacts.
3. **Qualify volume-to-cortex projection.** Support explicit MNI6 and MNI2009c
   source identities to fsaverage and fsLR through validated registration-fusion
   assets and the existing volume route as needed. Add explicit ribbon sampling
   as a separate method. Use released assets when permitted; audit their licenses
   independently of neuroatlas's code license. Backend dependency choices remain
   open pending oracle comparisons; do not require Python merely for data access.
4. **Complete common mixed workflows.** Add CIFTI dscalar/dlabel support and
   directed surface-to-volume atlas rasterization. Preserve subcortex. Evaluate
   CIVET surface correspondence next to cover the third major neuromaps family.
5. **Broaden volumetric and specialist coverage.** Inventory demand and published
   evidence for MNI152Lin, MNI symmetric variants, and ICBM2009 variants; connect
   qualified routes to the existing adult hubs. Follow with cerebellar SUIT and
   its separate flatmap projection, then cohort-specific MNIInfant/pediatric/dHCP
   mappings and newer surface families such as onavg. None are interchangeable
   aliases or automatically qualified because TemplateFlow distributes them.

The first three steps are the next coherent release goal. Later steps are
prioritized candidates, not commitments to manufacture every possible mapping.
[TemplateFlow catalog](https://github.com/templateflow/templateflow),
[SUIT projection methods](https://www.diedrichsenlab.org/imaging/suit_flatmap.htm).

## Acceptance evidence

Freeze per-method thresholds before candidate measurements. Reuse V1's immutable
artifact, checksum, cache/offline, provenance, and retained-failure discipline.

- Exercise every advertised direction, hemisphere, density/grid, and data type;
  missing assets or unsupported semantics fail before producing an output.
- Independently compare surface results with pinned Workbench and projection
  results with pinned neuromaps/CBIG or Workbench, using the same asset bytes.
  Reference agreement tests implementation, not anatomical truth by itself.
- Use constant fields, analytic sphere fields, physical-coordinate volume ramps,
  asymmetric hemisphere markers, small labels, and missing-data boundaries to
  expose direction, indexing, interpolation, and coverage mistakes.
- Report lost labels, per-label area changes, coverage, smoothing, and applicable
  area-weighted error. Distinguish scalar values from extensive quantities; choose
  conservation requirements per declared semantics. Do not demand impossible
  lossless downsampling or volume/surface round trips.
- Inspect sulcal/label boundary overlays, medial walls, and both hemispheres on
  native and inflated display surfaces. For projection, inspect white/pial
  sampling support over the exact source volume. Bind every review to artifacts.
- Use held-out anatomical references where available; identify dependent labels
  honestly. Record AI and human review separately. Passing image comparisons,
  numerical agreement, and anatomical accuracy are distinct claims.
- Demonstrate public API examples on actual atlas labels and continuous maps,
  with verified downloads and offline replay. Publish a coverage matrix and QA
  report for each supported route, including limitations and citations.

## Next action

The 0.2.0 slice implements representation-safe planning, the four pinned ordinary
fsaverage 164k / fsLR 32k routes, exact MNI6/MNI2009c population cortical
projection, and separately declared aligned ribbon sampling. Broad aliases,
other densities, CIFTI, directed backprojection and specialist coverage remain
future work. The implementation Mote continues to track that broader scope.

Read [the source-bound release evidence](../data-raw/surface-transforms-v1/qualification/devbox-20261006/final/index.md)
and [the machine handoff](resume-neuroatlas.md) for qualification and engineering
status. The historical strict Workbench comparison still fails in three of four
ordinary cases. Preserve its threshold and failed evidence; native interpolation
agreement does not establish Workbench equivalence, anatomical accuracy, area
conservation or an inverse. AI figure inspection and human review are recorded
separately. The per-asset license audit governs downloads independently of the
package code license.

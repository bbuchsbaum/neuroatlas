# Transform quality validation, 2026-10-07

Prospective protocol, frozen before this campaign's measurements. This expands
the evidence for the released operators; it does not change release gates or
reclassify earlier failures. Source baseline: c1dc2a4.

## Evidence and limits

1. Numerical agreement is not anatomical accuracy. Original exhaustive native
   oracle results and failed strict Workbench cases remain authoritative for
   their stated scope.
2. Harvard-Oxford labels in the two MNI templates are dependent, upstream mapped
   references. Regional overlap and boundary distances are diagnostic only.
3. Schaefer labels were generated in fsaverage6 and projected to fsLR and MNI
   (CBIG README at 634f676630929a71297852d01dd92a287103e861). Agreement with
   these labels tests published pipeline compatibility, not anatomical truth.
4. T1 alignment measures reuse the registration contrast. T2 is an additional
   contrast only; its provenance must be audited before calling it held out.
5. No independently paired manual landmarks or expert ratings have been
   obtained. Do not claim a ground-truth anatomical accuracy bound.

## Locked comparisons

Volume: both released MNI6/MNI2009c image pullbacks at 1 and 2 mm. Compare
identity resampling with the released warp using identical interpolation and
target grids. Report brain-mask Dice, symmetric mean boundary distance and
95th-percentile boundary distance (mm); all cortical HOCPA and subcortical
HOSPA labels' Dice, boundary distance, centroid distance and volume bias.
Include all nonzero keys present in either reference; missing labels score
zero Dice and undefined boundary distance, never disappear from summaries.
Report whole-brain and regional intensity correlation, and deformation
Jacobian/displacement distributions and dense inverse consistency in brain
and a 3 mm exterior shell. Count nonpositive/nonfinite Jacobians explicitly.
Use SimpleITK independently to read/sample the published H5; verify a public
R API application on real data before attributing the diagnostics to runtime.

Surface: four directed hemisphere/density routes, Schaefer 100/400/1000
7-network labels. Use full meshes, actual cortical masks, exact pinned
registration spheres and native public API. Compare with Workbench
BARYCENTRIC categorical aggregate voting and record availability separately.
Also compare ADAP_BARY_AREA using pinned group-average vertex areas as a
method-sensitivity experiment, not an equivalence gate. Evaluate against
published destination labels by names, not assuming common numeric IDs.
Report disagreement counts/fractions and destination-area-weighted fractions,
every parcel's Dice and area change, missing labels, boundary proximity of
disagreements. Continuous sulcal-depth/curvature maps on fsaverage test the
two downsampling routes against BARYCENTRIC and ADAP_BARY_AREA. Upsampling
uses the published fsLR categorical maps. Masks and label zero are distinct.

Projection: use the published fsLR Schaefer labels as a dependent external
processing comparison against projection of the published MNI6 Schaefer
atlas. Evaluate both hemispheres at 100/400/1000 parcels, with the same
per-parcel and coverage diagnostics. This measures practical population
mapping differences, not individual-subject cortical localization accuracy.

## Interpretation fixed in advance

No new overall anatomical PASS threshold is justified by the current
references. Report all cases, identity-relative changes, worst ten regions,
and the number of regions that worsen, including discordant metrics.
Scientific accuracy remains unestablished without independent anatomical
references. Hard engineering failures are nonfinite mapped brain coordinates,
nonpositive in-brain Jacobians, unexpected label IDs, or lost labels. Retain
and explain failures; do not tune thresholds or exclude inconvenient regions.
Boundary distances are Euclidean distances between voxel/vertex boundary
centres, with grid/mesh limitations stated. Surface sphere distances measure
boundary proximity, not anatomical millimetres or geodesic distances.

## Review and reproducibility

Hash inputs, scripts, protocol, operator bytes and results; record exact
software versions. Use a fresh attempt directory; never overwrite frozen
release receipts. Raw upstream assets stay in an external work directory.
Save CSV/JSON metrics plus fixed-window visual comparisons and worst-region
panels. Automated/AI inspection is explicitly distinct from human expert
review. Provide an unrated review form with visible case identifiers and
separate numerical findings; expert anatomical review remains pending.

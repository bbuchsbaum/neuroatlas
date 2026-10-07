# Projection diagnostics and supplementary method comparison

The public `projection_diagnostics()` accessor now distinguishes per-map
sampling loss from subsequent surface-resampling loss, keeps absent source keys
and supported zero separate, and reports hemisphere mismatches using explicit
metadata. The six real projections retain exactly the previous values and
availability. Independent Python counts verify every source voxel, sampled
vertex and target vertex count in the new diagnostic tables.

The supplementary [protocol](protocol.md) was frozen before measurements.
All six cases use the same pinned CBIG revision, original MNI6 1 mm volumes,
official lookup tables and exact fsaverage 164k/fsLR 32k geometry as the
[earlier campaign](../transform-quality-v1/README.md). Current runtime values
remain unchanged; this comparison does not replace the original release gates.

## Result

Both native aggregate and largest-contributor labels have zero value and
availability disagreements with Workbench 1.5.0 BARYCENTRIC across all six
cases. This is implementation agreement, not anatomical accuracy.

Neither largest-contributor selection nor Workbench area-aware aggregate/largest
voting passes the no-label-loss and no-opposite-hemisphere criteria across the
six cases. All tested variants lose the same three Schaefer1000 source parcels.
All right-hemisphere 1000-parcel variants retain three left-labelled vertices.
No alternative has been admitted as a new qualified runtime method.

| Method | Left median parcel Dice | Right median parcel Dice | Lost 1000-parcel keys |
|---|---:|---:|---|
| Barycentric aggregate | 0.6550 | 0.6369 | 214; 710, 903 |
| Barycentric largest | 0.6593 | 0.6387 | 214; 710, 903 |
| Area-aware aggregate | 0.6600 | 0.6491 | 214; 710, 903 |
| Area-aware largest | 0.6559 | 0.6387 | 214; 710, 903 |

These Dice scores describe agreement with the same-revision published fsLR
atlas, a dependent processing reference. They do not measure independent
anatomical correctness. A modestly better median does not pass the retention
gate. See [all 36 method cases](evidence-20261007/method-summary.csv).

## Where support is lost

All 500 hemisphere source labels survive initial fsaverage sampling. Keys 214,
710 and 903 have only 3, 1 and 67 sampled vertices respectively. Each connects
to just one supported fsLR cortex vertex in the released operator. Their total
contributor weights on supported target cortex are 0.0698, 0.4118 and 0.1586.
Key 903 also contributes to 21 masked target vertices. Changing ordinary voting
or using the tested area-aware voting does not recover these parcels. Forcing
keys into unrelated vertices would not establish a valid correspondence.

The [support table](evidence-20261007/lost-support.csv) binds these counts to the
operator identities. Richer sampling support, alternative target support or
densities, and independently reviewed anatomy need further investigation.
The original Mote `bd-01M4AZXRPXE47K2EZNN1GYHBVP` remains open for that work;
its diagnostic portion is implemented. Independent expert review and atlas
reference auditing remain separate Motes.

## Reproduction

Use the qualified engine build and cached inputs from the earlier campaign.
The output directories must be new so failed attempts remain available.

```sh
Rscript data-raw/projection-diagnostics-v1/export.R QUALITY_WORK NEW_EXPORT
python data-raw/projection-diagnostics-v1/compare.py \
  QUALITY_WORK NEW_EXPORT PINNED_SURFACE_INPUTS NEW_COMPARISON
Rscript data-raw/projection-diagnostics-v1/support.R \
  QUALITY_WORK NEW_EXPORT NEW_COMPARISON/lost-support.csv
```

Python requirements for the retained run: numpy 1.26.4 and nibabel 5.3.2.
Workbench is an external qualification tool; it is not a new package dependency.
The [export receipt](evidence-20261007/receipt.json) and
[comparison report](evidence-20261007/report.json) retain driver, protocol,
implementation, engine and output hashes. Raw binary/GIFTI derivatives remain
in the campaign workspace and can be regenerated; tables and counts are retained
here. Input licenses and attribution remain those of the original release's
surface/projection manifests and CBIG assets.

Method reference: [Workbench label resampling](https://www.humanconnectome.org/software/workbench-command/-label-resample).

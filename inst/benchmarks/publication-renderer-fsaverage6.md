# Publication renderer fsaverage6 receipt

Date: 2026-08-05

The deterministic CPU backend was composed through `plot_brain()` at the final
publication contract of 3000 by 1875 pixels and 300 dpi. The fixture used the
bundled fsaverage6 inflated surfaces, Schaefer-100 annotation cortex masks,
curvature computed on corresponding white geometry, canonical lateral and
medial cameras, and a synthetic signed continuous scalar field.

- Output: `/tmp/neuroatlas-fsaverage6-publication-3000x1875.png`
- Dimensions: 1875 by 3000 by 3 RGB channels
- PNG bytes: 638,964
- SHA-256: `ecc2b5c24a1c535242761c834ea1a9e3d7c5cd5c0d5bfa5b42de25ffa9154fae`
- Colorbar source: overlay
- Palette: `vik`
- Limits: exactly `[-3, 3]`
- Breaks: `[-3, -1, 0, 1, 3]`
- Left mask: 37,476 cortex and 3,486 medial-wall vertices
- Right mask: 37,471 cortex and 3,491 medial-wall vertices
- Wall-clock time: 37.50 seconds including package loading, curvature, four
  900 by 560 panels at 2x supersampling, and final ragg composition
- Maximum resident set size: 1,806,516,224 bytes; peak memory footprint:
  873,042,432 bytes (`/usr/bin/time -l`, host-qualified)

The artifact was visually inspected. It contains no parcel, mesh, or occlusion
lines; lateral and medial anatomy are distinct; the annotation-defined medial
wall is neutral; curvature remains visible under the overlay; anterior and
posterior marks are present; and the legend title, signed endpoints, zero, and
threshold marks are legible. The synthetic field intentionally forms broad
bands so threshold interpolation and the anatomy underlay can be inspected.

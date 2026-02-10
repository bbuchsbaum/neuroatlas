# neuroatlas 0.1.0.9000

* Fixed white gaps ("shards") in `plot_brain()` surface rendering caused by
  inconsistent triangle winding in some meshes.
* Added `silhouette*` and `network_border*` options to `plot_brain()` for
  improved boundary styling (silhouette outline and between-network borders).
* Improved `plot_brain()` aesthetics with smoother boundary rendering
  (`border_geom = "path"`) and an optional normal-based shading overlay
  (`shading*`, `fill_alpha`).

# neuroatlas 0.1.0

* Initial CRAN submission
* Added support for multiple neuroimaging atlases:
  - Schaefer cortical parcellations (100-1000 parcels, 7/17 networks)
  - Glasser multi-modal parcellation (360 regions)
  - FreeSurfer ASEG subcortical segmentation
  - Olsen medial temporal lobe atlas
* Integrated TemplateFlow support for standardized templates
* Added visualization support via ggseg and echarts4r
* Implemented atlas operations: ROI extraction, data reduction, resampling
* Added comprehensive vignettes and documentation

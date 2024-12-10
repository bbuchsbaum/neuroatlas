<package_overview>
neuroatlas: R package for neuroimaging atlas operations
Primary uses: Loading brain templates, accessing standard atlases, manipulating brain parcellations
Core capabilities: Template loading, atlas operations, coordinate transformations
</package_overview>

<template_functions>
# Template Loading Functions

<function>
get_template(name = "MNI152NLin2009cAsym", desc = "brain", resolution = 1, suffix = "T1w")
# Access standardized neuroimaging templates from TemplateFlow
</function>

<supported_templates>
- MNI152NLin2009cAsym (default)
- MNI152NLin6Asym
- OASIS30ANTs
- NKI
- WHS
</supported_templates>

<template_variants>
- brain: Brain-extracted template
- head: Full head template
- mask: Binary brain mask
- probseg: Probability maps (GM/WM/CSF)
</template_variants>

<example>
# Load standard MNI template
template <- get_template()

<return type="neuroim2::NeuroVol">

# Get 2mm resolution template
template_2mm <- get_template(resolution = 2)

# Load brain mask
mask <- get_template_brainmask()
</example>
</template_functions>

<atlas_functions>
# Atlas Operations

## Glasser Atlas
<function>
get_glasser_atlas(outspace = NULL)
# Load Glasser multi-modal cortical parcellation
</function>

## Schaefer Atlas
<function>
get_schaefer_atlas(parcels = c("100","200","300","400","500","600","800","1000"),
                   networks = c("7","17"), resolution = c("1","2"),
                   outspace = NULL)
# Load Schaefer cortical parcellation
</function>

## Surface Atlas
<function>
get_schaefer_surfatlas(parcels = "400", networks = "17", surf = c("inflated", "white", "pial"))
# Load surface-based Schaefer atlas
</function>

<example>
# Load Glasser atlas
glasser <- get_glasser_atlas()

# Load Schaefer atlas with 400 parcels and 17 networks
schaefer <- get_schaefer_atlas(parcels = "400", networks = "17")

# Load surface-based atlas
surf_atlas <- get_schaefer_surfatlas(parcels = "400", networks = "17", surf = "inflated")
</example>
</atlas_functions>

<utility_functions>
# Utility Functions

## Atlas Manipulation
<function>
merge_atlases(atlas1, atlas2)
# Combine two brain atlases into unified atlas

get_roi(atlas, label = NULL, id = NULL)
# Extract specific regions from atlas
</function>

## Visualization
<function>
ggseg_schaefer(atlas, vals, thresh = c(0,0), interactive = TRUE)
# Create interactive visualization of Schaefer atlas

plot_glasser(atlas, ...)
# Visualize Glasser atlas
</function>

<example>
# Merge two atlases
merged <- merge_atlases(atlas1, atlas2)

# Extract ROI
roi <- get_roi(atlas, label = "hippocampus")

# Visualize atlas
ggseg_schaefer(atlas, values, thresh = c(-2, 2))
</example>
</utility_functions>

<common_workflows>
# Common Workflows

1. Template-based analysis:
</common_workflows>

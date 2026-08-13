# Atlas Constructor and Validator

A canonical constructor for \`atlas\` objects shared across loaders.

\`new_atlas()\` and \`new_surfatlas()\` assemble the list structure that
every volumetric and surface atlas in neuroatlas must conform to:
\`name\`, \`atlas\` (or \`lh_atlas\`/\`rh_atlas\`), \`ids\`, \`labels\`,
\`orig_labels\`, \`hemi\`, \`network\`, \`cmap\`, and a pre-built
\`roi_metadata\` tibble. They also attach canonical \`atlas_ref\`
identity and provenance metadata via \`.attach_atlas_ref()\` /
\`.attach_atlas_provenance()\` so every loader produces consistently
shaped output.

\`validate_atlas()\` performs cheap structural checks and raises a typed
\`neuroatlas_error_invalid_atlas\` condition via \[cli::cli_abort()\]
when the object is missing required fields or has inconsistent vector
lengths.

These helpers are the preferred way for loaders to construct atlas
objects; external code should continue to use the documented
\`get\_\*\_atlas()\` entry points and rarely needs to call these
directly.

## Usage

``` r
new_atlas(
  name,
  atlas,
  ids,
  labels,
  orig_labels = NULL,
  hemi = NULL,
  network = NULL,
  cmap = NULL,
  subclass = character(),
  extra = list(),
  ref,
  artifacts = NULL,
  history = NULL
)

new_surfatlas(
  name,
  lh_atlas,
  rh_atlas,
  ids,
  labels,
  surf_type,
  surface_space,
  orig_labels = NULL,
  hemi = NULL,
  network = NULL,
  cmap = NULL,
  subclass = character(),
  extra = list(),
  ref,
  artifacts = NULL,
  history = NULL
)
```

## Arguments

- name:

  Human-readable atlas name (e.g. \`"Schaefer-200-7networks"\`).

- atlas:

  For volumetric atlases, a \`NeuroVol\` or \`ClusteredNeuroVol\`
  object.

- ids:

  Integer vector of region IDs.

- labels:

  Character vector of region labels (same length as \`ids\`).

- orig_labels:

  Optional character vector of original/full labels (defaults to
  \`labels\`).

- hemi:

  Optional character vector of hemisphere designations (\`"left"\`,
  \`"right"\`, or \`NA\`).

- network:

  Optional character vector of network assignments (for atlases that
  define networks, e.g. Schaefer).

- cmap:

  Optional data frame / matrix with three RGB columns.

- subclass:

  Character vector of subclasses prepended to \`"atlas"\` (e.g.
  \`c("schaefer", "volatlas")\`).

- extra:

  Named list of extra fields to merge into the returned atlas object
  (e.g. \`list(space = "MNI152NLin6Asym")\`). Reserved names are not
  overwritten.

- ref:

  An \`atlas_ref\` object created with \[new_atlas_ref()\].

- artifacts:

  Optional artifacts tibble built via \`.new_atlas_artifact()\`.

- history:

  Optional history tibble built via \`.new_atlas_history()\`.

- lh_atlas, rh_atlas:

  For surface atlases, the per-hemisphere \`LabeledNeuroSurface\`
  objects.

- surf_type:

  Surface type string (e.g. \`"pial"\`). Surface atlases only.

- surface_space:

  Surface template space (e.g. \`"fsaverage6"\`). Surface atlases only.

## Value

A list with class \`c(subclass, "atlas")\` (or \`c(subclass,
"surfatlas", "atlas")\` for surface atlases) containing the canonical
atlas fields plus \`roi_metadata\` and attached provenance.

## See also

\[new_atlas_ref()\], \[atlas_provenance()\], \[roi_metadata()\].

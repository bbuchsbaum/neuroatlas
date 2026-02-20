# Atlas Reference Metadata

Structured provenance and space metadata for atlas objects.

## Usage

``` r
new_atlas_ref(
  family,
  model,
  representation = c("volume", "surface", "derived"),
  template_space = NA_character_,
  coord_space = NA_character_,
  resolution = NA_character_,
  density = NA_character_,
  provenance = NA_character_,
  source = NA_character_,
  lineage = NA_character_,
  confidence = c("exact", "high", "approximate", "uncertain"),
  notes = NA_character_
)
```

## Arguments

- family:

  Atlas family identifier (e.g., \`"schaefer"\`, \`"glasser"\`).

- model:

  Atlas model identifier (e.g., \`"Schaefer2018"\`).

- representation:

  Atlas representation (\`"volume"\`, \`"surface"\`, or \`"derived"\`).

- template_space:

  Template/grid identifier (e.g., \`"MNI152NLin2009cAsym"\`,
  \`"fsaverage6"\`).

- coord_space:

  Coordinate-space identifier (e.g., \`"MNI152"\`, \`"MNI305"\`).

- resolution:

  Optional resolution descriptor (e.g., \`"1mm"\`).

- density:

  Optional surface density descriptor (e.g., \`"41k"\`).

- provenance:

  URL, DOI, or short source identifier.

- source:

  Source key used by the loader.

- lineage:

  Optional derivation/projection note.

- confidence:

  Confidence tier: \`"exact"\`, \`"high"\`, \`"approximate"\`, or
  \`"uncertain"\`.

- notes:

  Optional free-text notes.

## Value

An object of class \`"atlas_ref"\`.

# Register an Atlas Spec

Adds (or replaces) an entry in the internal atlas registry.

## Usage

``` r
register_atlas(
  id,
  label,
  family,
  loader,
  default_space = NA_character_,
  representation = c("volume", "surface", "derived"),
  aliases = character(),
  description = NA_character_
)
```

## Arguments

- id:

  Canonical atlas id (character scalar).

- label:

  Human-readable label.

- family:

  Atlas family (e.g. \`"schaefer"\`, \`"glasser"\`).

- loader:

  Name of the loader function (character scalar); resolved via
  \[getExportedValue()\] / \[get()\] at dispatch time.

- default_space:

  Default template space the loader returns when given no explicit
  \`outspace\` argument.

- representation:

  One of \`"volume"\`, \`"surface"\`, or \`"derived"\`.

- aliases:

  Optional character vector of accepted aliases.

- description:

  Optional free-text description.

## Value

Invisibly, the spec list.

## Details

Registered as an internal helper — external packages that want to plug a
new atlas into \[get_atlas()\] dispatch can call this from their
\`.onLoad()\` hook.

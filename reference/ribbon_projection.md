# Declare Aligned White/Pial Ribbon Sampling

Declares ordered white and pial coordinates in the same physical frame
as the source volume. Alignment and vertex ordering are assertions by
the caller; this function does not fit or verify a registration.
Inflated display surfaces must not be supplied. Equal-weight nodes
include both endpoints. This method differs from Workbench
voxel-intersection ribbon mapping.

## Usage

``` r
ribbon_projection(domain, white, pial, cortex, frame, n_samples = 5L)
```

## Arguments

- domain:

  Exact surface domain for the ordered anatomical coordinates.

- white, pial:

  Numeric vertex-by-three matrices in RAS millimetres.

- cortex:

  Logical inclusion vector matching the domain's cortical mask.

- frame:

  Exact shared physical-frame identifier.

- n_samples:

  Integer node count, at least two. Nodes are equally spaced along each
  white-to-pial segment; categorical nodes vote with smallest-key ties.

## Value

A \`SurfaceProjection\` with coordinate hashes and caller-declared
alignment provenance. Numerical sampling qualification does not
establish anatomical registration accuracy.

## Examples

``` r
if (FALSE) { # \dontrun{
projection <- ribbon_projection(domain, white, pial, cortex,
  frame = "subject-01-scanner-RAS", n_samples = 5
)
} # }
```

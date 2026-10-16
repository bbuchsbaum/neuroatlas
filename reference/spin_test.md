# Spin Test for Spatial Correlation Significance

Tests whether the spatial correlation between two parcellated brain maps
is stronger than expected by chance, using the spin-test framework of
Alexander-Bloch et al. (2018). Parcel centroids on each hemisphere's
sphere are randomly rotated and reassigned to the nearest original
centroid of the same hemisphere, generating a null distribution of
correlations that preserves spatial autocorrelation and hemisphere
membership.

## Usage

``` r
spin_test(
  map1,
  map2,
  atlas,
  n_perm = 1000L,
  cor_method = c("pearson", "spearman", "kendall"),
  sphere = NULL,
  seed = NULL,
  hemi_rotation = c("mirror", "independent")
)
```

## Arguments

- map1:

  Numeric vector of parcel values (length K, aligned to atlas parcels).

- map2:

  Numeric vector of parcel values (length K, aligned to atlas parcels).

- atlas:

  A surface atlas object (class `"surfatlas"`) with `lh_atlas` and
  `rh_atlas` fields, or any atlas whose geometry can provide sphere
  coordinates. When `sphere` is a K x 3 centroid matrix, `atlas$hemi`
  must give each parcel's hemisphere (`"left"` or `"right"`).

- n_perm:

  Integer. Number of spin permutations. Default: 1000.

- cor_method:

  Character. Correlation method passed to
  [`cor`](https://rdrr.io/r/stats/cor.html). One of `"pearson"`,
  `"spearman"`, or `"kendall"`. Default: `"pearson"`.

- sphere:

  Optional sphere geometry. Either a list with elements `lh` and `rh`,
  each an N x 3 matrix of sphere vertex coordinates for that hemisphere
  (in the hemisphere's own sphere coordinate frame), or a K x 3 matrix
  of parcel centroids in `atlas$ids` order. If `NULL` (default), sphere
  coordinates are fetched via
  [`load_surface_template`](https://bbuchsbaum.github.io/neuroatlas/reference/load_surface_template.md)`(..., "sphere")`
  using the atlas's `surface_space`.

- seed:

  Optional integer seed for reproducibility.

- hemi_rotation:

  Character. How the right-hemisphere rotation relates to the
  left-hemisphere rotation in each permutation. `"mirror"` (default, as
  in Alexander-Bloch et al. 2018) draws one random rotation for the left
  hemisphere and applies its reflection across the mid-sagittal (x = 0)
  plane to the right hemisphere, so homologous locations rotate
  homologously. `"independent"` draws a separate random rotation for
  each hemisphere. In both cases each hemisphere is rotated on its own
  sphere and parcels are only reassigned within their own hemisphere.

## Value

A list of class `"spin_test"` with:

- observed:

  The observed correlation between `map1` and `map2`.

- null_distribution:

  Numeric vector of length `n_perm` containing null correlations.

- p_value:

  Two-sided p-value: proportion of null correlations with absolute value
  \>= the observed absolute correlation.

- n_perm:

  Number of permutations performed.

- hemi_rotation:

  The hemisphere rotation scheme used.

- permutations:

  K x `n_perm` integer matrix; column `i` gives, for each parcel, the
  index of the parcel whose `map2` value it receives in permutation `i`.

## Details

The algorithm:

1.  Compute parcel centroids on each hemisphere's sphere by averaging
    the sphere coordinates of that hemisphere's vertices belonging to
    each parcel.

2.  For each permutation, generate a uniform random 3D rotation matrix
    (from the Haar measure on SO(3)) for the left hemisphere and derive
    the right-hemisphere rotation according to `hemi_rotation`. Rotate
    the centroids of each hemisphere with that hemisphere's rotation,
    then reassign each rotated centroid to its nearest original centroid
    in the *same* hemisphere using
    [`nn`](https://rdrr.io/pkg/Rnanoflann/man/nn.html).

3.  Compute the correlation of `map1` with the permuted `map2` (values
    shuffled according to the reassignment).

4.  The p-value is
    `(sum(abs(null) >= abs(observed)) + 1) / (n_perm + 1)`.

Nearest-centroid reassignment can map several rotated parcels to the
same original parcel (sampling with replacement), as in the original
Alexander-Bloch et al. (2018) procedure.

## References

Alexander-Bloch, A. F., et al. (2018). On testing for spatial
correspondence between maps of human brain structure and function.
*NeuroImage*, 178, 540-551.

## Examples

``` r
if (FALSE) { # \dontrun{
atlas <- get_schaefer_surfatlas(parcels = "100", networks = "7")
K <- length(atlas$ids)
map1 <- rnorm(K)
map2 <- map1 + rnorm(K, sd = 0.5)
res <- spin_test(map1, map2, atlas, n_perm = 500, seed = 42)
print(res)
} # }
```

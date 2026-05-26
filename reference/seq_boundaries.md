# Create *FOREST* and *OWNER* object from *PARCA* for a Sequoia project

Aggregates parcel geometries to build forest and owner boundaries.

## Usage

``` r
seq_boundaries(dirname = ".", tol = 10, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- tol:

  `numeric`; buffer tolerance in meter used to close small gaps. Any gap
  narrower than `2 * tol` is filled, causing polygons on both sides to
  be merged.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

A named list

## Details

**About `tol`** The gap-closing logic is based on a French forestry rule
stating that a forest split by a path or narrow road is legally
considered a single continuous stand

To reflect this, small linear gaps are removed using the sequence
*buffer -\> union -\> unbuffer*, applied with the distance given by
`tol`. Any gap narrower than `2 * tol` is closed.

**Output layers** The function produces six layers:

- `FOREST_poly` : Forest boundaries with aggregated surface. **`tol` is
  applied** to remove small gaps.

- `FOREST_line` : Forest boundaries as linestrings.

- `FOREST_point` : Centroids of forest polygons.

- `OWNER_poly` : Boundaries aggregated per owner. **`tol` is *not*
  applied** to avoid merging polygons across owners.

- `OWNER_line` : Owner boundaries as linestrings.

- `OWNER_point` : Centroids of owner polygons.

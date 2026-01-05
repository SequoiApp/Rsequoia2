# Remove Overlaps in a *PARCA* Layer

This function resolves overlapping geometries in a *PARCA* layer by
keeping a single geometry for each overlapping area, based on INSEE
total surface. It then aggregates geometries by `IDU` and normalizes the
result.

## Usage

``` r
remove_overlaps(parca)
```

## Arguments

- parca:

  An `sf` object, typically produced by
  [`seq_parca()`](https://mucau.github.io/Rsequoia2/reference/seq_parca.md),
  containing cadastral parcels.

## Value

An `sf` object corresponding to the input `parca` layer, with overlaps
resolved, geometries aggregated by `IDU`, and normalized.

## Details

**Workflow:**

1.  **Clean topology**: Fix minor topological issues using
    `clean_topology()` with `snap` and
    [`break`](https://rdrr.io/r/base/Control.html).

2.  **Detect overlaps**: Identify geometries that are duplicated due to
    overlaps.

3.  **Rank INSEE**: Compute total `SURF_CA` per INSEE and assign a rank,
    so that INSEE with largest total surface has highest priority.

4.  **Select overlaps**: For each overlapping geometry, keep only the
    entity with the highest INSEE rank.

5.  **Combine with non-overlaps**: Merge resolved overlaps with
    geometries that do not overlap.

6.  **Aggregate by IDU**: Merge geometries with the same `IDU` and
    preserve the first occurrence of non-numeric attributes.

7.  **Normalize**: Apply
    [`seq_normalize()`](https://mucau.github.io/Rsequoia2/reference/seq_normalize.md)
    to standardize the layer.

Temporary columns used internally (`geom_key`, `overlap_id`,
`insee_rank`) are removed in the output.

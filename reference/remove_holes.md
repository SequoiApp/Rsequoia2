# Remove Holes in a *PARCA* Layer

This function fills holes in a *PARCA* layer by adding geometries
corresponding to the missing areas.

## Usage

``` r
remove_holes(parca)
```

## Arguments

- parca:

  An `sf` object, typically produced by
  [`seq_parca()`](https://mucau.github.io/Rsequoia2/reference/seq_parca.md),
  containing cadastral parcels.

## Value

An updated `sf` object based on `parca`, with holes replaced by
geometries without attributes.

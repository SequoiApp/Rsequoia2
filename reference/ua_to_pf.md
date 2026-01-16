# Aggregate *UA* surfaces at the `parcelle` field level

Aggregates corrected surface areas from a *UA* layer by grouping rows
according to the parcel unit field, as defined by the configuration
returned by
[`seq_field()`](https://mucau.github.io/Rsequoia2/reference/seq_field.md).

## Usage

``` r
ua_to_pf(ua)
```

## Arguments

- ua:

  `sf` object containing analysis units

## Value

`sf` object where polygons are aggregated at the *PF* level, with
corrected surfaces.

## Details

This function is typically used internally by
[`seq_parcels()`](https://mucau.github.io/Rsequoia2/reference/seq_parcels.md),
but may also be called directly when a PF-level summary of a UA dataset
is required.

## See also

[`ua_to_sspf()`](https://mucau.github.io/Rsequoia2/reference/ua_to_sspf.md),
[`seq_parcels()`](https://mucau.github.io/Rsequoia2/reference/seq_parcels.md),
[`seq_field()`](https://mucau.github.io/Rsequoia2/reference/seq_field.md),
[`seq_normalize()`](https://mucau.github.io/Rsequoia2/reference/seq_normalize.md)

# Aggregate *UA* surfaces at the `sspf` field level

Aggregates corrected surface areas from a *UA* layer by grouping rows
according to the `sspf` field, as defined by the configuration returned
by
[`seq_field()`](https://mucau.github.io/Rsequoia2/reference/seq_field.md).

## Usage

``` r
ua_to_sspf(ua)
```

## Arguments

- ua:

  `sf` object containing analysis units

## Value

An `sf` object where polygons and descriptive fields are aggregated at
the *SSPF* level.

## Details

In addition to summing surfaces, descriptive fields are preserved.

This function is used internally by
[`seq_parcels()`](https://mucau.github.io/Rsequoia2/reference/seq_parcels.md),
but may be called directly when a SSPF-level summary of UA data is
needed.

## See also

[`ua_to_pf()`](https://mucau.github.io/Rsequoia2/reference/ua_to_pf.md),
[`seq_parcels()`](https://mucau.github.io/Rsequoia2/reference/seq_parcels.md),
[`seq_field()`](https://mucau.github.io/Rsequoia2/reference/seq_field.md),
[`seq_normalize()`](https://mucau.github.io/Rsequoia2/reference/seq_normalize.md)

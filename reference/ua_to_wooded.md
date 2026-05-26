# Aggregate *UA* surfaces by wooded / non-wooded status

Aggregates corrected surface areas from a *UA* layer by grouping rows
according to the wooded status field, as defined by the configuration
returned by
[`seq_field()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_field.md).

## Usage

``` r
ua_to_wooded(ua)
```

## Arguments

- ua:

  `sf` object containing analysis units.

## Value

An `sf` object where polygons are aggregated by wooded status, with
corrected surfaces.

## Details

This function is typically used internally by
[`seq_wooded()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_wooded.md),
but may also be called directly when a summary of wooded versus
non-wooded areas is required.

## See also

[`seq_wooded()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_wooded.md),
[`seq_field()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_field.md),
[`seq_normalize()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_normalize.md)

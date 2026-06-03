# Aggregate *UA* surfaces by occupation status

Aggregates corrected surface areas from a *UA* layer by grouping rows
according to DGD-submitted and wooded status.

## Usage

``` r
ua_to_occupation(ua, verbose = TRUE)
```

## Arguments

- ua:

  `sf` object containing analysis units.

- verbose:

  `boolean` if `TRUE` print messages

## Value

An `sf` object aggregated by occupation status.

## See also

[`seq_occupation()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_occupation.md),
[`seq_field()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_field.md),
[`seq_normalize()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_normalize.md)

# Summarize a DRIAS index by period and month

Compute a summary (mean, sum, etc.) of a specific DRIAS index for two
periods (H1 and H3) by month and calculate the difference.

## Usage

``` r
get_drias_abstract(tables, field, fun = mean)
```

## Arguments

- tables:

  A data.frame returned by
  [`get_drias_table()`](https://mucau.github.io/Rsequoia2/reference/get_drias_table.md).
  Must contain columns `Periode`, `Mois` (between 1 and 12) and the
  target index.

- field:

  Character. Name of the index to summarize (e.g., "NORTAV").

- fun:

  Function to summarize the values (default: `mean`).

## Value

A data.frame with columns:

- Mois:

  Month number (between 1 and 12).

- H1_FIELD:

  Summary of H1 period for the selected index.

- H3_FIELD:

  Summary of H3 period for the selected index.

- EC_FIELD:

  Difference H3 - H1 for the selected index.

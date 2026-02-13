# Compute monthly precipitation, evapotranspiration and water balance from DRIAS projections

Reads a DRIAS climate projection `.txt` file and computes the mean
monthly precipitation (P), potential evapotranspiration (ETP), and water
balance (P - ETP) for each projection period.

## Usage

``` r
drias_etp(txt)
```

## Arguments

- txt:

  `character(1)`. Path to a DRIAS `.txt` file downloaded from the [DRIAS
  portal](https://www.drias-climat.fr/)

## Value

A `data.frame` with one row per month and period

## Details

Invalid climate points (where precipitation or evapotranspiration are
entirely equal to zero across all months) are automatically excluded to
avoid biased averages.

## See also

[`drias_read_table()`](https://mucau.github.io/Rsequoia2/reference/drias_read_table.md),
[`drias_read_metadata()`](https://mucau.github.io/Rsequoia2/reference/drias_read_metadata.md),
[`drias_ombro()`](https://mucau.github.io/Rsequoia2/reference/drias_ombro.md)

## Examples

``` r
if (FALSE) { # \dontrun{
txt <- "indicesRACMO22E_CNRM-CM5.txt"
etp <- drias_etp(txt)
head(etp)
} # }
```

# Get SIREN from pattern

Search SIREN identifiers by matching normalized patterns against company
names.

## Usage

``` r
get_siren(pattern, verbose = TRUE)
```

## Arguments

- pattern:

  Character vector of search patterns.

- verbose:

  Logical; print messages and results.

## Value

A data.frame of matching records (invisible).

## Examples

``` r
if (FALSE) { # \dontrun{
get_siren("TOTAL")
get_siren(c("BANQUE", "POSTALE"))
} # }
```

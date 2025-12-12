# Load table definitions from the Sequoia configuration

Internal helper used by
[`seq_normalize()`](https://mucau.github.io/Rsequoia2/reference/seq_normalize.md)
to determine which fields belong to a given table. Table definitions are
stored in `inst/config/seq_tables.yaml`.

## Usage

``` r
seq_table(table = NULL, filepath = NULL)
```

## Arguments

- table:

  `character` or `NULL`; If provided, must match a key in
  `inst/config/seq_tables.yaml`.

- filepath:

  `character` or `NULL`; override for the path to the default YAML
  configuration file (`inst/config/seq_fields.yaml`). Used mainly for
  testing.

## Value

A character vector of field keys for the selected table.

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available tables
names(seq_table())

# Load field keys for the "parcelle" table
seq_table("parca")
} # }
```

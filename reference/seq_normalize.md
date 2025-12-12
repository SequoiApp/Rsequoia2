# Normalize a data frame according to Sequoia table definitions

`seq_normalize()` standardizes the structure of a `data.frame` using the
Sequoia configuration stored in `inst/config/seq_fields.yaml` and
`seq_tables.yaml`.

## Usage

``` r
seq_normalize(x, table)
```

## Arguments

- x:

  `data.frame` to normalize.

- table:

  `character` A single table name as defined in
  `config/seq_tables.yaml`.

## Value

A normalized `data.frame` matching the structure required by the
selected Sequoia table.

## Details

The normalization process applies, in order:

1.  **Field renaming** using configured aliases

2.  **Field selection**: add missing required fields and drop invalid
    ones

3.  **Type coercion** to the classes defined in the configuration

4.  **Column reordering** according to the table definition

This ensures that the resulting data frame matches exactly the schema
expected for the selected Sequoia table.

**Field aliases** defined in `seq_fields.yaml` allow multiple column
names (e.g. `"foret"`, `"id"`) to be mapped to the canonical field name
(e.g. `"IDENTIFIANT"`). Aliases are automatically detected and replaced
during normalization.

For example, if the configuration contains:

    id:
      name: "IDENTIFIANT"
      alias: ["id", "foret"]

Then:

    names(df)
    #> c("foret", "contenance")

    df <- seq_normalize(df, "parca")

    names(df)
    #> c("IDENTIFIANT", "SURF_CAD")

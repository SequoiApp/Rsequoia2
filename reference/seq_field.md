# Load field definitions from the Sequoia configuration

Internal helper used by
[`seq_normalize()`](https://mucau.github.io/Rsequoia2/reference/seq_normalize.md)
to access field metadata defined in `inst/config/seq_fields.yaml`. When
`field` is `NULL`, the full configuration list is returned. Otherwise,
the corresponding field definition is extracted.

## Usage

``` r
seq_field(field = NULL, filepath = NULL)
```

## Arguments

- field:

  `character` or `NULL`; If provided, must match a key in
  `inst/config/seq_fields.yaml`.

- filepath:

  `character` or `NULL`; override for the path to the default YAML
  configuration file (`inst/config/seq_fields.yaml`). Used mainly for
  testing.

## Value

A list describing all fields, or a single field definition.

## Examples

``` r
if (FALSE) { # \dontrun{
# Return all field definitions
seq_field()

# All avaialbe fields
names(seq_field())

# Return the definition of a specific field
seq_field("identifiant")
} # }
```

# Load layer config from the Sequoia configuration

Internal helper used to determine layer info like name or extension are
stored in `inst/config/seq_layerss.yaml`.

## Usage

``` r
seq_layer(pattern = NULL, filepath = NULL)
```

## Arguments

- pattern:

  `character` or `NULL`; regular-expression pattern used to filter keys.
  If `NULL`, all keys defined in the configuration are returned.

- filepath:

  `character` or `NULL`; optional override for the path to the YAML
  configuration file. Mainly used for testing purposes.

## Value

A list describing matched layer

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available tables
names(seq_table())

# Load field keys for the "parcelle" table
seq_table("parca")
} # }
```

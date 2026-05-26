# Helper to find layer configuration keys

This helper function searches for keys defined in
`inst/config/seq_layers.yaml`. Keys are matched using a
regular-expression `pattern`.

## Usage

``` r
get_keys(pattern = NULL, reduce = TRUE, filepath = NULL)
```

## Arguments

- pattern:

  `character` or `NULL`; regular-expression pattern used to filter keys.
  If `NULL`, all keys defined in the configuration are returned.

- reduce:

  `logical`; if `TRUE` (default), reduce each matching key.

- filepath:

  `character` or `NULL`; optional override for the path to the YAML
  configuration file. Mainly used for testing purposes.

## Value

A character vector of matching keys. If `reduce = TRUE`, the returned
keys are the reduced forms. If `pattern = NULL`, all configuration keys
are returned.

## Details

**Reduction process** Configuration keys typically follow a
three/four-part structure such as `"v.seq.parca.poly"` or
`"r.ortho.irc"`. When `reduce = TRUE`, each key is split on `"."` and
only the third element is returned (s). This provides a short, semantic
identifier for internal use.

    "v.seq.parca.poly" -> "parca"
    "r.ortho.irc"      -> "irc"

If multiple full keys reduce to the same name, the function aborts to
prevent ambiguity.

## Examples

``` r
if (FALSE) { # \dontrun{
# List all available layer keys
get_keys()

# List full keys matching "parca"
get_keys("parca", reduce = FALSE)

# Return reduced keys (3rd element of each dotted key)
get_keys("parca", reduce = TRUE)
} # }
```

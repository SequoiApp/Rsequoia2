# Get Rsequoia2 cache configuration

Reads the cache registry from \`seq_cache.yaml\` and returns either the
full configuration or the resolved cache information for one key.

## Usage

``` r
seq_cache(key = NULL, filepath = NULL)
```

## Arguments

- key:

  \`character\` Cache key to retrieve. If \`NULL\`, the full YAML
  configuration is returned.

- filepath:

  \`character\` Optional path to a cache configuration file. Mostly
  useful for tests.

## Value

If \`key = NULL\`, a \`list\` containing the full cache configuration.
Otherwise, a \`list\` with \`label\`, \`path\`, and \`description\`.

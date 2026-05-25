# Report Rsequoia2 cache usage

Prints a small CLI summary of the total cache size and the size of each
configured cache folder.

## Usage

``` r
seq_cache_report(filepath = NULL)
```

## Arguments

- filepath:

  \`character\`. Optional path to a cache configuration file. Mostly
  useful for tests. If \`NULL\`, the package cache configuration is
  used.

## Value

Invisibly returns a \`data.frame\` with cache key, label, path, size in
bytes, and formatted size.

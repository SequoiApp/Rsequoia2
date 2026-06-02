# Clear Rsequoia2 cache

Deletes one or all configured Rsequoia2 cache folders after user
confirmation.

## Usage

``` r
seq_cache_clear(key = NULL, filepath = NULL, ask = TRUE)
```

## Arguments

- key:

  `character`. Cache key to clear. If `NULL`, all configured caches are
  cleared.

- filepath:

  `character`. Optional path to a cache configuration file. Mostly
  useful for tests.

- ask:

  `logical`. Ask confirmation before deleting cache files.

## Value

Invisibly returns `TRUE`.

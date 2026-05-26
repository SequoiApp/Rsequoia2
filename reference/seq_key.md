# Resolve a layer configuration key

Resolves a user-provided key or pattern against the keys defined in
`inst/config/seq_layers.yaml`.

## Usage

``` r
seq_key(key = NULL, allow_multiple = FALSE)
```

## Arguments

- key:

  `character` Search pattern to resolve.

- allow_multiple:

  `logical` If `TRUE`, allow and return multiple matches.

## Value

A character vector of matching configuration keys.

## Details

Partial matching is used. If no key matches, the function aborts. If
multiple keys match and `allow_multiple = FALSE`, the function aborts.

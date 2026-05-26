# Validate department codes

Checks that department codes are non-empty and exist in the COG
reference. Values are converted to character, padded to two digits, and
deduplicated.

## Usage

``` r
check_dep(dep)
```

## Arguments

- dep:

  Department code vector.

## Value

A normalized `character` vector of valid department codes.

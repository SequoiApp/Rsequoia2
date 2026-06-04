# Validate insee codes

Checks that insee codes are non-empty and exist in the COG reference.
Values are converted to character, padded to two digits, and
deduplicated.

## Usage

``` r
check_insee(insee)
```

## Arguments

- insee:

  insee code vector.

## Value

A normalized \`character\` vector of valid insee codes.

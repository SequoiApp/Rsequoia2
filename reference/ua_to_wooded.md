# Aggregate \_UA\_ surfaces by wooded / non-wooded status

Aggregates corrected surface areas from a \_UA\_ layer by grouping rows
according to the wooded status field, as defined by the configuration
returned by \[seq_field()\].

## Usage

``` r
ua_to_wooded(ua)
```

## Arguments

- ua:

  \`sf\` object containing analysis units.

## Value

An \`sf\` object where polygons are aggregated by wooded status, with
corrected surfaces.

## Details

This function is typically used internally by \[seq_wooded()\], but may
also be called directly when a summary of wooded versus non-wooded areas
is required.

## See also

\[seq_wooded()\], \[seq_field()\], \[seq_normalize()\]

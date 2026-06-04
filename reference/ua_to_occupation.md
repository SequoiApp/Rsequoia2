# Aggregate \_UA\_ surfaces by occupation status

Aggregates corrected surface areas from a \_UA\_ layer by grouping rows
according to DGD-submitted and wooded status.

## Usage

``` r
ua_to_occupation(ua, verbose = TRUE)
```

## Arguments

- ua:

  \`sf\` object containing analysis units.

- verbose:

  \`boolean\` if \`TRUE\` print messages

## Value

An \`sf\` object aggregated by occupation status.

## See also

\[seq_occupation()\], \[seq_field()\], \[seq_normalize()\]

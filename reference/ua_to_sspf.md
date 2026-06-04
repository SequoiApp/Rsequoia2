# Aggregate \_UA\_ surfaces at the \`sspf\` field level

Aggregates corrected surface areas from a \_UA\_ layer by grouping rows
according to the \`sspf\` field, as defined by the configuration
returned by \[seq_field()\].

## Usage

``` r
ua_to_sspf(ua)
```

## Arguments

- ua:

  \`sf\` object containing analysis units

## Value

An \`sf\` object where polygons and descriptive fields are aggregated at
the \_SSPF\_ level.

## Details

In addition to summing surfaces, descriptive fields are preserved.

This function is used internally by \[seq_parcels()\], but may be called
directly when a SSPF-level summary of UA data is needed.

## See also

\[ua_to_pf()\], \[seq_parcels()\], \[seq_field()\], \[seq_normalize()\]

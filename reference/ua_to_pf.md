# Aggregate \_UA\_ surfaces at the \`parcelle\` field level

Aggregates corrected surface areas from a \_UA\_ layer by grouping rows
according to the parcel unit field, as defined by the configuration
returned by \[seq_field()\].

## Usage

``` r
ua_to_pf(ua)
```

## Arguments

- ua:

  \`sf\` object containing analysis units

## Value

\`sf\` object where polygons are aggregated at the \_PF\_ level, with
corrected surfaces.

## Details

This function is typically used internally by \[seq_parcels()\], but may
also be called directly when a PF-level summary of a UA dataset is
required.

## See also

\[ua_to_sspf()\], \[seq_parcels()\], \[seq_field()\],
\[seq_normalize()\]

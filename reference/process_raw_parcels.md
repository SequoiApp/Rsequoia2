# Process raw parcel data into structured format

Cleans, normalizes, and structures raw parcel data by identifying main
parcels, subdivisions, and adding calculated fields like surface area.

## Usage

``` r
process_raw_parcels(df)
```

## Arguments

- df:

  Raw \`data.frame\` from \[get_raw_parcels()\]

## Value

Processed \`data.frame\` with normalized fields, row types, IDs, and
calculated columns

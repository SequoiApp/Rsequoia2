# Repair wooded status from DGD status

Forces wooded status to FALSE when a surface is not submitted to DGD.

## Usage

``` r
ua_repair_wooded(ua, verbose = TRUE)
```

## Arguments

- ua:

  \`sf\` object containing UA polygons.

- verbose:

  Logical. Should messages be printed?

## Value

The repaired \`ua\` object.

# Check non-wooded surface threshold

Checks that non-wooded submitted surfaces do not exceed 10 DGD surfaces.

## Usage

``` r
ua_check_non_wooded_threshold(ua, threshold = 0.1, verbose = TRUE)
```

## Arguments

- ua:

  \`sf\` object containing UA polygons.

- threshold:

  Maximum allowed non-wooded ratio. Defaults to \`0.10\`.

- verbose:

  Logical. Should messages be printed?

## Value

\`TRUE\` if the threshold is respected, otherwise \`FALSE\`.

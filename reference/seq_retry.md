# Add retry capability to seq\_\* function

Internal helper used to retry download when failing

## Usage

``` r
seq_retry(expr, times = 3, wait = 0.5, verbose = TRUE)
```

## Arguments

- expr:

  Code to capture

- times:

  Number of retry

- wait:

  Time to wait between retry

- verbose:

  `logical` If `TRUE`, display messages.

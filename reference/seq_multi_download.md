# Download multiple files with retry

Internal wrapper around
[`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
used to download cached resources. Existing files are skipped unless
`overwrite = TRUE`; failed downloads are retried up to `max_tries`.

## Usage

``` r
seq_multi_download(
  urls,
  destfiles,
  overwrite = FALSE,
  verbose = TRUE,
  max_tries = 3
)
```

## Arguments

- urls:

  `character`; Remote file URLs.

- destfiles:

  `character`; Local destination paths.

- overwrite:

  `logical(1)`; If `TRUE`, re-download existing files.

- verbose:

  `logical(1)`; If `TRUE`, show download progress and messages.

- max_tries:

  `integer(1)`; Maximum number of download attempts.

## Value

Invisibly returns `TRUE` if all required files are available.

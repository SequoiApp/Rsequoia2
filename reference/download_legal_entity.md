# Download and cache cadastral data for legal entities ("Personnes Morales")

This function downloads the official "Personnes Morales" datasets
published by the French Ministry of Economy and Finance. These datasets
contain cadastral and property information (parcels and buildings) owned
by legal entities. The data are automatically cached locally to avoid
repeated downloads.

## Usage

``` r
download_legal_entity(cache = seq_cache("legal_entity")$path, verbose = TRUE)
```

## Arguments

- cache:

  `character`; Storage directory. Defaults to the user cache directory.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

`character`;

## Examples

``` r
if (FALSE) { # \dontrun{
download_legal_entity(c(29, "08"))
} # }
```

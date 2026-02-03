# Get COG administrative datasets

Download and cache French COG datasets (communes, departments, regions)
from data.gouv.fr. Cached data are reused unless `update = TRUE`.

## Usage

``` r
get_cog(cache = NULL, update = FALSE, verbose = TRUE)
```

## Arguments

- cache:

  Cache directory. If `NULL`, uses package cache directory.

- update:

  Force re-download even if cached.

- verbose:

  Display progress messages.

## Value

Named list of data frames:

- `com`: communes

- `dep`: departments

- `reg`: regions

## Examples

``` r
if (FALSE) { # \dontrun{
cog <- get_cog()
cog <- get_cog(update = TRUE)
} # }
```

# Create a forest matrice for legal entity from insee code

Generates a forest matrice used to store general forest information
(e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral attributes
(`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`) for legal
entity.

## Usage

``` r
get_legal_entity(x, cache = NULL, verbose = TRUE)
```

## Arguments

- x:

  `character`; Code(s) INSEE or code(s) department (see
  \[Rsequoia2::get_cog())

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

## Examples

``` r
if (FALSE) { # \dontrun{
pm <- get_legal_entity(c("29158", "08185"))
} # }
```

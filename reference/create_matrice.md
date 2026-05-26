# Create a forest matrice

Generates a default forest matrice as excel file used to store general
forest information (e.g., `IDENTIFIANT`, `PROPRIETAIRE`) and cadastral
attributes (`CODE_INSEE`, `PREFIXE`, `SECTION`, `NUMERO`, `LIEU_DIT`).

## Usage

``` r
create_matrice(
  dirname = ".",
  id = "MY_FOREST",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  `character` Path to the directory. Defaults to the current working
  directory.

- id:

  `character` Identifier of the forest. Typically the name of the forest

- overwrite:

  `logical` If `TRUE`, filename is overwritten.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Invisibly returns the path to the created file.

## Examples

``` r
if (FALSE) { # \dontrun{
create_matrice("~/matrice.xlsx", overwrite = TRUE)
} # }
```

# Extract forest ID from a matrice.xlsx file

Searches for a single Excel file matching `*_matrice.xlsx` in a
directory, reads its `IDENTIFIANT` column, and returns the unique forest
ID.

## Usage

``` r
get_id(dirname = ".", verbose = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Invisibly returns a single forest ID (character scalar).

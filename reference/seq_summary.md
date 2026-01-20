# Summarize information for a Sequoia folder

Generate synthetic table from Sequoia layers and write them as `.xlsx`
inside the current Sequoia dir. Useful for redaction of management
document.

## Usage

``` r
seq_summary(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

`list` of `data.frame`

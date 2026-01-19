# Update `R_SEQUOIA` projet to `Rsequoia2`

Update `R_SEQUOIA` projet to `Rsequoia2`

## Usage

``` r
seq1_update(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Directory where the `R_SEQUOIA` files are located.
  Defaults to the current working directory.

- verbose:

  `logical` If `TRUE`, display progress messages.

## Value

Invisibly returns a named vector of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly when no layer are found.

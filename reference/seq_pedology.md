# Generate pedology polygon layer and associated PDF reports

Retrieves pedology polygon features intersecting the project area,
writes the resulting layer to disk and downloads associated pedology PDF
reports into the project directory.

## Usage

``` r
seq_pedology(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  `character` Path to the project directory. Defaults to the current
  working directory.

- verbose:

  `logical`; whether to display informational messages. Defaults to
  `TRUE`.

- overwrite:

  `logical`; whether to overwrite existing files. Defaults to `FALSE`.

## Value

Invisibly returns a named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly when no pedology features are found.

## Details

Pedology polygon features are retrieved using
[`get_pedology()`](https://mucau.github.io/Rsequoia2/reference/get_pedology.md).

If no pedology features intersect the project area, the function returns
`NULL` invisibly and no file is written.

When pedology features are present, the polygon layer is written to disk
using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
with the key `"v.sol.pedo.poly"`. Associated UCS PDF reports are then
downloaded into `dirname` using
[`get_pedology_pdf()`](https://mucau.github.io/Rsequoia2/reference/get_pedology_pdf.md).

## See also

[`get_pedology()`](https://mucau.github.io/Rsequoia2/reference/get_pedology.md),
[`get_pedology_pdf()`](https://mucau.github.io/Rsequoia2/reference/get_pedology_pdf.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)

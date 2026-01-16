# Generate graphic road layers for a Sequoia project

Create graphic road layers from a completed cadastral gaps layer.

## Usage

``` r
seq_graphic_road(dirname = ".", verbose = TRUE, overwrite = FALSE)
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
Returns `NULL` invisibly when no features are created.

## Details

Features are created from a completed cadastral gaps layer obtained by
using
[`seq_vides()`](https://mucau.github.io/Rsequoia2/reference/seq_vides.md).

## See also

[`seq_vides()`](https://mucau.github.io/Rsequoia2/reference/seq_vides.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)

# Generate regional layers for a Sequoia project

Retrieves official IGN regional datasets intersecting the project area
and writes the resulting layers to disk.

## Usage

``` r
seq_region(
  dirname = ".",
  types = c("ser", "ser_ar", "rfn", "rfd", "zp"),
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dirname:

  `character` Path to the project directory. Defaults to the current
  working directory.

- types:

  `character` Vector of region types to retrieve. Possible values are
  `"ser"`, `"ser_ar"`, `"rfn"`, `"rfd"` and `"zp"`. Defaults to all
  available types.

- verbose:

  `logical`; whether to display informational messages. Defaults to
  `TRUE`.

- overwrite:

  `logical`; whether to overwrite existing files. Defaults to `FALSE`.

## Value

Invisibly returns a named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).
Returns `NULL` invisibly if no regional layer is written.

## Details

Regional datasets are retrieved using
[`get_region()`](https://mucau.github.io/Rsequoia2/reference/get_region.md)
based on the project area defined by the PARCA polygon.

Each regional layer is written to disk using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
with a dedicated output key corresponding to the requested region type.

If no feature is found for a given type, the corresponding layer is not
written.

## See also

[`get_region()`](https://mucau.github.io/Rsequoia2/reference/get_region.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)

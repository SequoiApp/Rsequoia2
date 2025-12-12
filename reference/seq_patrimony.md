# Download patrimony vector layers for a Sequoia project

Downloads one or several vector layers with `frheritage` for `pat`
layer(s) of a Sequoia project.

## Usage

``` r
seq_patrimony(
  dirname = ".",
  buffer = 500,
  key = get_keys("pat"),
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- buffer:

  `numeric`; Buffer around `x` (in **meters**) used to enlarge the
  download area.

- key:

  `character`; List of layer identifiers to download. If not provided,
  the function uses `get_keys("pat")` to automatically select all
  patrimony layers defined in the Sequoia configuration
  (`inst/config/seq_layers.yaml`)

- verbose:

  `logical`; If `TRUE`, display messages.

- overwrite:

  `logical`; If `TRUE`, file is overwritten.

## Value

A named list of file paths written by
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md),
one per layer.

## Details

This function is a convenience wrapper looping over
[`get_patrimony()`](https://mucau.github.io/Rsequoia2/reference/get_patrimony.md),
allowing the user to download all products in one call and automatically
write them to the project directory using
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md).

For each value in `key`, the function attempts to query the
corresponding MNHN layer using
[`get_patrimony()`](https://mucau.github.io/Rsequoia2/reference/get_patrimony.md).

- If the layer contains features, it is written to the project directory
  via
  [`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)
  and recorded as a successful download.

- If the layer contains no features, it is skipped and marked as empty.

## See also

[`get_patrimony()`](https://mucau.github.io/Rsequoia2/reference/get_patrimony.md),
[`seq_write()`](https://mucau.github.io/Rsequoia2/reference/seq_write.md)

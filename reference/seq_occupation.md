# Create occupation layer from *UA* for a Sequoia project

Reads the UA polygon layer, aggregates surfaces by occupation status,
and writes the derived occupation layer to the project directory.

## Usage

``` r
seq_occupation(dirname = ".", verbose = TRUE, overwrite = FALSE)
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

An invisible file path to the exported occupation layer.

## See also

[`ua_to_occupation()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_to_occupation.md),
[`seq_read()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_read.md),
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md)

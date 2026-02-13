# Create *PF* and *SSPF* object from *UA* for a Sequoia project

This function reads the UA polygon layer (`v.seq.ua.poly`) of a Sequoia
project, aggregates surfaces at the `parcelle` and `sous-parcelle` level
and writes four derived layers:

## Usage

``` r
seq_parcels(dirname = ".", verbose = FALSE, overwrite = FALSE)
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

A named list of four file paths, corresponding to the exported PF and
SSPF polygon and line layers.

## Details

Created layers are automatically written to the project directory using
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md),
respecting the `dirname`, `overwrite`, and `verbose` parameters.

The function performs two levels of spatial aggregation ande create 4
layers:

1.  **PF (Parcelle)** Rows of the UA are grouped using the `parcelle`
    field, and corrected surface areas are summed. A polygon and border
    lines layer are created.

2.  **SSPF (Sous-parcelle)** Rows of the UA are grouped using the
    `parcelle` and `sous-parcelle` field, and corrected surface areas
    are summed. A polygon and border lines layer are created.

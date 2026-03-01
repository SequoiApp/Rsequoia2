# Download pedology PDF reports from INRA soil maps

Downloads pedological PDF documents associated with UCS identifiers from
the INRA soil map repository.

## Usage

``` r
get_pedology_pdf(id_ucs, dirname = NULL, verbose = TRUE)
```

## Arguments

- id_ucs:

  `character` used to identify pedology reports. It can be got by using
  `get_pedology()$id_ucs`.

- dirname:

  `character`; directory where the PDF will be saved. Defaults to
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)

- verbose:

  `logical`. If `TRUE`, display progress messages.

## Value

Invisibly returns the normalized path to `out_dir`. Returns `NULL`
invisibly if no valid `id_ucs` is found.

## Details

The function needs unique UCS identifiers typically got from the
`id_ucs` field of `pedology`, builds download URLs pointing to the INRA
soil map repository, and downloads the corresponding PDF documents.

Existing files are skipped unless `overwrite = TRUE`. All user feedback
is handled via the `cli` package and can be silenced by setting
`verbose = FALSE`.

## See also

[`get_pedology()`](https://sequoiapp.github.io/Rsequoia2/reference/get_pedology.md)

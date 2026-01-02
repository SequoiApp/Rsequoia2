# Download pedology PDF reports from INRA soil maps

Downloads pedological PDF documents associated with UCS identifiers from
the INRA soil map repository.

## Usage

``` r
get_pedology_pdf(
  pedology,
  out_dir = "pedology_pdf",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- pedology:

  An object (typically an `sf` object get by
  [`get_pedology()`](https://mucau.github.io/Rsequoia2/reference/get_pedology.md))
  containing an `id_ucs` field used to identify pedology reports.

- out_dir:

  Output directory where PDF files are saved.

- overwrite:

  `logical`; whether to overwrite existing files. Defaults to `FALSE`.

- verbose:

  `logical`. If `TRUE`, display progress messages.

## Value

Invisibly returns the normalized path to `out_dir`. Returns `NULL`
invisibly if no valid `id_ucs` is found.

## Details

The function extracts unique UCS identifiers from the `id_ucs` field of
`pedology`, builds download URLs pointing to the INRA soil map
repository, and downloads the corresponding PDF documents.

Existing files are skipped unless `overwrite = TRUE`. All user feedback
is handled via the `cli` package and can be silenced by setting
`verbose = FALSE`.

## See also

[`get_pedology()`](https://mucau.github.io/Rsequoia2/reference/get_pedology.md)

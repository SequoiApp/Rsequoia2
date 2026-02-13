# Download sylvoecoregion PDF reports from INF repository

Downloads PDF documents associated with SER identifiers from the IFN
repository.

## Usage

``` r
get_ser_pdf(ser, out_dir = "ser_pdf", overwrite = FALSE, verbose = TRUE)
```

## Arguments

- ser:

  An object (typically an `sf` object get by `get_ifn("ser")`)
  containing an `codeser` field used to identify sylvoecoregion reports.

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

The function extracts unique SER identifiers from the `codeser` field of
`ser`, builds download URLs pointing to the INRA soil map repository,
and downloads the corresponding PDF documents.

Existing files are skipped unless `overwrite = TRUE`. All user feedback
is handled via the `cli` package and can be silenced by setting
`verbose = FALSE`.

## See also

[`get_ifn()`](https://sequoiapp.github.io/Rsequoia2/reference/get_ifn.md)

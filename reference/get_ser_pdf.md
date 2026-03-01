# Download sylvoecoregion PDF reports from INF repository

Downloads PDF documents associated with SER identifiers from the IFN
repository.

## Usage

``` r
get_ser_pdf(id_ser, dirname = NULL, overwrite = FALSE, verbose = TRUE)
```

## Arguments

- id_ser:

  `character` used to identify pedology reports. It can be got by using
  `get_ifn("ser")$codeser`.

- dirname:

  Output directory where PDF files are saved.

- overwrite:

  `logical`; whether to overwrite existing files. Defaults to `FALSE`.

- verbose:

  `logical`. If `TRUE`, display progress messages.

## Value

Invisibly returns the normalized path to `out_dir`. Returns `NULL`
invisibly if no valid `id_ucs` is found.

## Details

The function needs unique SER identifiers typically got from the
`codeser` field of `ser`, builds download URLs pointing to the INRAE
soil map repository, and downloads the corresponding PDF documents.

Existing files are skipped unless `overwrite = TRUE`. All user feedback
is handled via the `cli` package and can be silenced by setting
`verbose = FALSE`.

## See also

[`get_ifn()`](https://sequoiapp.github.io/Rsequoia2/reference/get_ifn.md)

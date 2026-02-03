# Download BRGM "BD Charm 50" harmonised geology ZIP files

Downloads BRGM ZIP archives for one french department. Files are stored
in a persistent cache directory (`R_user_dir("Rsequoia2", "cache")`).

## Usage

``` r
download_brgm(
  dep,
  source = "carhab",
  cache = NULL,
  verbose = FALSE,
  overwrite = FALSE
)
```

## Arguments

- dep:

  `character` or `numeric`; Department code (see
  [`get_cog()`](https://mucau.github.io/Rsequoia2/reference/get_cog.md)).

- source:

  `character` Source use to download geology from BRGM. Must be one of:

  - `"carhab"` : geological data used to created the `CarHab` dataset ;

  - `"bdcharm50"` : geological maps, vectorized and harmonised at
    1:50,000 scale.

- cache:

  `character`; Storage directory. Defaults to the user cache directory
  (see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, overwrite zipfile.

## Value

Character vector of directories where ZIPs are stored.

## Details

Existing ZIP files are never re-downloaded.

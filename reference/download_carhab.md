# Download CarHab geology archive

Downloads CarHab ZIP archives for one or more French departments and
stores them in the Rsequoia2 cache.

## Usage

``` r
download_carhab(
  dep,
  cache = seq_cache("carhab")$path,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dep:

  `character` or `numeric`; Department code(s), such as `"08"` or `8`.
  Codes are checked against
  [`get_cog()`](https://sequoiapp.github.io/Rsequoia2/reference/get_cog.md).

- cache:

  `character`; Directory where ZIP archives are stored. Defaults to the
  Rsequoia2 BD Charm 50 cache directory, see
  [`seq_cache()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache.md).

- verbose:

  `logical`; If `TRUE`, display progress messages.

- overwrite:

  `logical`; If `TRUE`, re-download archives even when they already
  exist in `cache`.

## Value

Invisibly returns a named `character` vector of local ZIP file paths,
with department codes as names.

## Details

CarHab is a harmonised geological map database produced by BRGM used to
created the `CarHab` dataset of PatriNat. The downloaded ZIP files are
cached and reused on later calls unless `overwrite = TRUE`.

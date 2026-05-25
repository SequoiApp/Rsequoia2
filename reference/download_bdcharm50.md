# Download BRGM BD Charm 50 geology archives

Downloads BD Charm 50 ZIP archives for one or more French departments
and stores them in the Rsequoia2 cache.

## Usage

``` r
download_bdcharm50(
  dep,
  cache = seq_cache("bdcharm50")$path,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dep:

  \`character\` or \`numeric\`; Department code(s), such as \`"08"\` or
  \`8\`. Codes are checked against \[Rsequoia2::get_cog()\].

- cache:

  \`character\`; Directory where ZIP archives are stored. Defaults to
  the Rsequoia2 BD Charm 50 cache directory, see
  \[Rsequoia2::seq_cache()\].

- verbose:

  \`logical\`; If \`TRUE\`, display progress messages.

- overwrite:

  \`logical\`; If \`TRUE\`, re-download archives even when they already
  exist in \`cache\`.

## Value

Invisibly returns a named \`character\` vector of local ZIP file paths,
with department codes as names.

## Details

BD Charm 50 is a harmonised 1:50,000 geological map database produced by
BRGM from vectorised geological maps. The downloaded ZIP files are
cached and reused on later calls unless \`overwrite = TRUE\`.

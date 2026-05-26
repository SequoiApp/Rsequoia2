# Create geology layers for a Sequoia project from BRGM data

Uses the project's *PARCA* layer as the area of interest, downloads the
requested BRGM geology dataset(s), clips them to the parcel geometry,
and writes the resulting layers to the project directory with
[`seq_write()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_write.md).

## Usage

``` r
seq_geol(
  dirname = ".",
  key = NULL,
  cache = NULL,
  buffer = 100,
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- key:

  `character`. Optional geology layer identifier(s). If `NULL`, all
  available geology layers are created. Available layers are
  `"v.sol.carhab.poly"` and `"v.sol.bdcharm50.poly"`. Partial matching
  is supported through
  [`seq_key()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_key.md).

- cache:

  `character`; Optional cache directory. If `NULL`, the dataset-specific
  cache from
  [`seq_cache()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_cache.md)
  is used.

- buffer:

  `numeric`; Buffer distance, in meters, applied around `x` before
  spatial filtering. Default is `100`.

- verbose:

  `logical` If `TRUE`, display messages.

- overwrite:

  `logical` If `TRUE`, file is overwritten.

## Value

Invisibly returns a named `list` of file paths to the created layers.

## Details

**BD Charm 50** contains harmonised 1:50,000 geological map data from
BRGM. It is detailed and may contain finely split geological units.

**CarHab** is a simplified and harmonised reinterpretation of geological
formations into broader lithological classes, designed for ecological
modelling.

More info: <https://infoterre.brgm.fr/page/carhab-donnees-geologiques>

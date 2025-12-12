# Construct filepath for a given layer key

Builds the expected output filename for a given layer key by combining:
(1) the forest ID extracted from the local \*\_matrice.xlsx file, and
(2) the corresponding layer definition stored in
`inst/config/seq_layers.yaml`.

## Usage

``` r
get_path(key, dirname = ".", verbose = FALSE)
```

## Arguments

- key:

  `character` Name of a layer key to match against the entries defined
  in `inst/config/seq_layers.yaml`. (see *Details* for partial
  matching).

- dirname:

  `character` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  `logical` If `TRUE`, display messages.

## Value

Invisibly returns a single forest ID (character scalar).

## Details

The function resolves the input `key` using **partial matching** against
keys defined in `inst/config/seq_layers.yaml`.

If exactly **one** entry matches, it is selected : for example,
`key = "znieff1"` can be used to match `v.mnhn.znieff1.poly` because
only one key contain `"znieff1`

If **multiple** entries match, the user is shown the ambiguous options
and must provide a more specific key.

Note: **`seq_layers.yaml` is part of the package and should not be
modified.**

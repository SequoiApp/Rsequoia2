# Retrieve layer metadata from the Sequoia configuration

Resolves a layer key against the config defined in
`inst/config/seq_layers.yaml` and `inst/config/seq_path.yaml` and
returns the associated metadata: name, extension, filename, path,
fullpath

## Usage

``` r
seq_layer(key, verbose = FALSE)
```

## Arguments

- key:

  `character` Name of a layer key to match against the entries defined
  in `inst/config/seq_layers.yaml`. (see *Details* for partial
  matching).

- verbose:

  `logical` If `TRUE`, display messages.

## Value

A named list containing layer metadata, including:

- `key`: the resolved configuration key

- `name`: the layer name

- `ext`: the file extension

- `family`: the resolved namespace family

- `path`: the base directory for the layer

- `full_path`: the complete filesystem path to the layer

## Details

The function resolves the input `key` using **partial matching** against
the keys defined in `inst/config/seq_layers.yaml`.

- If exactly **one** entry matches, it is selected. For example,
  `key = "znieff1"` can be used to match `"v.mnhn.znieff1.poly"` when
  this is the only key containing `"znieff1"`.

- If **multiple** entries match, the function aborts and displays the
  ambiguous keys, prompting the user to provide a more specific key.

Note: **`seq_layers.yaml` is part of the package and must not be
modified.**

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve metadata for a layer
l <- seq_layer("parca")

l$full_path
} # }
```

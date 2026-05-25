# Generate regional layers for a Sequoia project

Retrieves official IGN regional datasets intersecting the project area
and writes the resulting layers to disk.

## Usage

``` r
seq_ifn(
  dirname = ".",
  key = get_keys("ifn"),
  verbose = TRUE,
  overwrite = FALSE
)
```

## Arguments

- dirname:

  \`character\` Path to the project directory. Defaults to the current
  working directory.

- key:

  \`character\`; List of ifn layer identifiers to download. If not
  provided, the function uses \`get_keys("ifn")\` to automatically
  select all MNHN layers defined in the Sequoia configuration
  (\`inst/config/seq_layers.yaml\`)

- verbose:

  \`logical\`; whether to display informational messages. Defaults to
  \`TRUE\`.

- overwrite:

  \`logical\`; whether to overwrite existing files. Defaults to
  \`FALSE\`.

## Value

Invisibly returns a named list of file paths written by \[seq_write()\].
Returns \`NULL\` invisibly if no regional layer is written.

## Details

Regional datasets are retrieved using \[get_ifn()\] based on the project
area defined by the PARCA polygon.

Each regional layer is written to disk using \[seq_write()\] with a
dedicated output key corresponding to the requested region k.

If no feature is found for a given k, the corresponding layer is not
written.

## See also

\[get_ifn()\], \[seq_write()\]

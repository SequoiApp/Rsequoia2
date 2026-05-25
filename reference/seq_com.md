# Generates commune polygon, line and point layers for a Sequoia project.

This function is a convenience wrapper around \[get_com_poly()\],
\[get_com_line()\] and \[get_com_point()\], allowing the user to
retrieve administrative boundary products and automatically write them
to the project directory using \[seq_write()\].

## Usage

``` r
seq_com(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Path to the project directory. Defaults to the current
  working directory.

- verbose:

  \`logical\` If \`TRUE\`, display messages.

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

## Value

A named list of file paths written by \[seq_write()\], one per commune
layer.

## Details

Both topological (full extent) and graphical (restricted extent)
representations are generated when relevant.

Commune layers are built from BDTOPO commune boundaries intersecting the
project area defined by the PARCA polygon.

The following layers are produced:

- Topological layers:

  Full commune geometry (polygon, boundary lines, centroids)

- Graphical layers:

  Line and point representations clipped to a reduced convex buffer
  around the project area, intended for cartographic display

Each layer is always written to disk using \[seq_write()\], even when it
contains no features (\`nrow == 0\`).

Informational messages are displayed to indicate whether a layer
contains features or is empty.

## See also

\[get_com_poly()\], \[get_com_line()\], \[get_com_point()\],
\[seq_write()\]

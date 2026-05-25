# Generates hydrographic polygon, line and point layers for a Sequoia project.

This function is a convenience wrapper around \[get_hydro_poly()\],
\[get_hydro_line()\] and \[get_hydro_point()\], allowing the user to
download all products in one call and automatically write them to the
project directory using \[seq_write()\].

## Usage

``` r
seq_hydro(dirname = ".", buffer = 1000, verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Directory where the matrice file is located. Defaults to
  the current working directory.

- buffer:

  \`numeric\`; Buffer around \`x\` (in \*\*meters\*\*) used to enlarge

- verbose:

  \`logical\` If \`TRUE\`, display messages.

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

## Value

A named list of file paths written by \[seq_write()\], one per
hydrographic layer.

## Details

Each hydrographic layer is always written to disk using \[seq_write()\],
even when it contains no features (\`nrow == 0\`).

Informational messages are displayed to indicate whether a layer
contains features or is empty.

## See also

\[get_hydro_poly()\], \[get_hydro_line()\], \[get_hydro_point()\],
\[seq_write()\]

# Generates vegetation polygon, line and point layers for a Sequoia project.

This function is a convenience wrapper around \[get_vege_poly()\],
\[get_vege_line()\] and \[get_vege_point()\], allowing the user to
download all products in one call and automatically write them to the
project directory using \[seq_write()\].

## Usage

``` r
seq_vege(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Directory where the matrice file is located. Defaults to
  the current working directory.

- verbose:

  \`logical\` If \`TRUE\`, display messages.

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

## Value

A named list of file paths written by \[seq_write()\], one per
vegetation layer.

## Details

Each vegetation layer is always written to disk using \[seq_write()\],
even when it contains no features (\`nrow == 0\`).

## See also

\[get_vege_poly()\], \[get_vege_line()\], \[get_vege_point()\],
\[seq_write()\]

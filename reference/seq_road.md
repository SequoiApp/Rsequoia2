# Generate road section layer for a Sequoia project

Retrieves road section line features intersecting and surrounding the
project area and writes the resulting layer to disk.

## Usage

``` r
seq_road(dirname = ".", buffer = 1000, verbose = TRUE, overwrite = FALSE)
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

Invisibly returns a named list of file paths written by \[seq_write()\].
Returns \`NULL\` invisibly when no features are found.

## Details

Road section line features are retrieved using \[get_road()\].

If no features are found, the function returns \`NULL\` invisibly and no
file is written.

When features are present, the layer is written to disk using
\[seq_write()\] with the key \`"v.road.line"\`.

## See also

\[get_road()\], \[seq_write()\]

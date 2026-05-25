# Generate cadastral gaps layer for a Sequoia project

Retrieves cadastral gaps features intersecting and surrounding the
project area and writes the resulting layer to disk.

## Usage

``` r
seq_voids(dirname = ".", verbose = TRUE, overwrite = FALSE)
```

## Arguments

- dirname:

  \`character\` Path to the project directory. Defaults to the current
  working directory.

- verbose:

  \`logical\`; whether to display informational messages. Defaults to
  \`TRUE\`.

- overwrite:

  \`logical\`; whether to overwrite existing files. Defaults to
  \`FALSE\`.

## Value

Invisibly returns a named list of file paths written by \[seq_write()\].
Returns \`NULL\` invisibly when no features are found.

## Details

Cadastral gaps are retrieved using \`get_voids()\`.

If no features are found, the function returns \`NULL\` invisibly and no
file is written.

When features are present, the layer is written to disk using
\[seq_write()\] with the key \`"v.cad.vides.poly"\`.

## See also

\[get_voids()\], \[seq_write()\]

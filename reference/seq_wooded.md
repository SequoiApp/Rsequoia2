# Create wooded-area layer from \_UA\_ for a Sequoia project

This function reads the UA polygon layer (\`v.seq.ua.poly\`) of a
Sequoia project, aggregates corrected surface areas by wooded /
non-wooded status, and writes the derived layer to the project
directory.

## Usage

``` r
seq_wooded(dirname = ".", verbose = FALSE, overwrite = FALSE)
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

An invisible named list containing the file path of the exported wooded
layer.

## Details

Created layers are automatically written using \[seq_write()\],
respecting the \`dirname\`, \`overwrite\`, and \`verbose\` parameters.

Rows of the UA layer are grouped according to the wooded status field
(\`is_wooded\`), and corrected surface areas are summed for each class.

## See also

\[ua_to_wooded()\], \[seq_read()\], \[seq_write()\], \[seq_field()\]

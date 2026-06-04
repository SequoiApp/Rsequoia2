# Create RGE ALTI layers for a Sequoia project

Uses the project's \_PARCA\_ layer to download and write RGE ALTI MNT,
MNS and/or MNH rasters.

## Usage

``` r
seq_rgealti(
  dirname = ".",
  key = c("mnt", "mns", "mnh"),
  buffer = 200,
  res = 1,
  crs = 2154,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dirname:

  \`character\` Directory where the matrice file is located. Defaults to
  the current working directory.

- key:

  \`character\`; RGE ALTI product(s) to create. One or more of
  \`"mnt"\`, \`"mns"\` and \`"mnh"\`.

- buffer:

  \`numeric\`; Buffer around \`x\` (in \*\*meters\*\*) used to enlarge
  the download area.

- res:

  \`numeric\`; resolution specified in the units of the coordinate
  system (see \[happign::get_wms_raster()\])

- crs:

  \`numeric\` or \`character\`; CRS of the returned raster (see
  \[happign::get_wms_raster()\])

- overwrite:

  \`logical\` If \`TRUE\`, file is overwritten.

- verbose:

  \`logical\`; If \`TRUE\`, display messages.

## Value

Invisibly returns a named \`character\` vector of output raster paths.

# Sequoia

``` r
knitr::opts_chunk$set(
  collapse = TRUE,
  out.width = "100%",
  dpi = 300,
  fig.width = 7.2916667,
  comment = "#>"
)
```

``` r
library(Rsequoia2)
library(tmap)
library(openxlsx2)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
```

## What is Sequoia

Sequoia is an opiniated way of dealing with forest management. It’s goal
is to :

- Facilitate downloading of data for forest management ;
- Normalize data (name and structure) ;
- Facilitate exploration of those data

## How it works

Sequoia always have those step :

- Setting a directory ;
- Creating an excel matrice ;
- Downloading data based on this matrice ;
- GIS part to create gestion unit ;
- Generating final layer

## Setting a directory

All `seq_*` function have `dirname = "."` argument which mean by default
it will use the current directory. The best practice is to create
directory and make it the default one for this R session.

For the example, will used a fictif forest named `"ECKMUHL"`.

``` r
# tempdir is used for the exampl
sequoia_dir <- "ECKMUHL"
dir.create(sequoia_dir)

# setting current dir to our sequoia dir
setwd(sequoia_dir)
```

## Creating an excel matrice

Sequoia offer mulitple ways of generating this fondamental file :

- Empty excel manually filled : see Cadastral Parcels article ;
- Using legal entity open data : see Cadastral Parcels - Legal entity
  article ;
- Generating file from pdf matrice of DGFIP

For the example, internal dataset is used

``` r
matrice <- read_xlsx(system.file("extdata/ECKMUHL_matrice.xlsx", package = "Rsequoia2"))

write_xlsx(matrice, file.path(sequoia_dir, "ECKMUHL_matrice.xlsx"))
```

## Downloading data

Sequoia offer many datasets :

- Cadastral data from DGFIP ;
- Environnemental data from MNHN ;
- Elevation data from IGN ;
- Orthophoto data from IGN ;
- Geological data from BRGM ;
- Hydrological data from BD Topo IGN ;
- Patrimony data from Atlas du Patrimoine ;
- …

To facilitate the downloading process, `seq_` function should be used.
Those function use spatialized cadastral parcels as base area.

If you want to customize folder structure, you can do it here

``` r
vdir <- file.path(sequoia_dir, "VECTOR")
rdir <- file.path(sequoia_dir, "RASTER")

seq_parca(vdir)
seq_mnhn(vdir)
seq_elevation(rdir)
seq_ortho(rdir)
seq_geol(vdir)
seq_hydro(vfdir)
seq_patrimony(vdir)
```

All this process can be done be the wrapper `seq_data()` :

``` r
seq_data(sequoia_dir)
```

Be aware that `seq_dir()` as a `config` arg that allow any user to
create customize directory. This config file as a .yaml that can be
generate with `seq_config()`.

For the exemple a fake yaml is used.

``` r
folder_str <- list(
  "seq_parca" = "PARCA",
  "seq_mnhn" = "ENVIRONNEMENT",
  "seq_elevation" = "RASTER",
  "seq_ortho" = "RASTER",
  "seq_geol" = "GEOL",
  "seq_hydro" = "TOPO",
  "seq_patrimony" = "PATRIMOINE"
) |> as.yaml()

seq_data(forest, config = folder_str)
```

## GIS part

…

## Finalizing layer

..

## How it can be used

Sequoia is created thinking that power user and friendly user will use
it. This is reflected by the name of function. All function starting
with `seq_*` are wrapper around low level function for friendly user.

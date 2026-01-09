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
#> Attempting to load the package cache ... No cache found.
#> Will try to reconfigure qgisprocess and build new cache ...
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

### Internal configuration

`Rsequoia2` relies on an internal configuration that maps keys to data
layers and their metadata. For example, the key `"r.sol.geol"` refers to
the geological raster layer named `GEOL.tif`.

All available keys can be listed with
[`seq_layer()`](https://mucau.github.io/Rsequoia2/reference/seq_layer.md).
To access metadata for a specific layer, use `seq_layer("r.sol.geol")`.

Note that full keys are not always required: `seq_layer("geol")` also
works.

\##`seq_*` functions

All main functions in `Rsequoia2` start with `seq_*` (e.g. seq_geol(),
seq_prsf(), …).

These functions: - Read the project cadastral parcels to defined area of
interest ; - Download the required data ; - Save them according to the
configuration (name and extension) ; - Return path(s) where the data are
stored.

*Note: for advanced users, each `seq_*` function has an equivalent
`get_*` function (e.g. `get_geol()`,
[`get_prsf()`](https://mucau.github.io/Rsequoia2/reference/get_prsf.md),
…). These can be used independently of the Sequoia workflow.*

### Reading data

[`seq_read()`](https://mucau.github.io/Rsequoia2/reference/seq_read.md)
uses the configuration to read any layer using its key (vector or
raster).

``` r
parca <- seq_read("parca")
dem <- seq_read("dem")
```

## What is a Sequoia process

Sequoia process always have those step :

- Setting a directory ;
- Creating parca matrice (excel with cadastrals parcels info);
- Downloading parca geometry;
- Downloading data based on parca;
- GIS part to create analysis units ;
- Generating final layer from analysis units

## Setting a directory

All `seq_*` function have `dirname = "."` argument, meaning they use the
current working directory by default. Best practice is to create a
dedicated directory and set it as the working directory for the R
session.

For the example, a forest named `"ECKMUHL"` is used:

``` r
sequoia_dir <- "ECKMUHL"
dir.create(sequoia_dir)

# setting current dir to our sequoia dir
setwd(sequoia_dir)
```

## Creating parca matrice

`Rsequoia2` provides several ways to generate this fundamental file:

- A manually filled empty Excel file (see Cadastral Parcels article)
- Using legal-entity open data (see Cadastral Parcels – Legal Entity
  article)
- Generating the file from DGFiP PDF matrices

## Downloading parca geometry

All data downloaded by `Rsequoia2` are based on cadastral parcels
(parca). The first mandatory step is therefore to download them using
[`seq_parca()`](https://mucau.github.io/Rsequoia2/reference/seq_parca.md).

``` r
seq_parca()
```

## Downloading data

`Rsequoia2` offer many datasets, which can be downloaded using `seq_*`
functions:

- Environnemental data from MNHN ;
- Elevation data from IGN ;
- Orthophoto data from IGN ;
- Geological data from BRGM ;
- Hydrological data from BD Topo IGN ;
- Patrimony data from Atlas du Patrimoine ;
- …

``` r
mnhn <- seq_mnhn()
```

As explained above, all data are saved in the current directory by
default. If you need a more structured folder layout, you can provide a
custom `dirname`.

In the example below, raster and vector data are stored in separate
directories:

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

## GIS part

…

## Finalizing layer

..

## How it can be used

Sequoia is created thinking that power user and friendly user will use
it. This is reflected by the name of function. All function starting
with `seq_*` are wrapper around low level function for friendly user.

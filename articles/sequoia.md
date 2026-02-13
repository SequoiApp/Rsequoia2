# Sequoia

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

## What is a Sequoia process

Sequoia process always have those step :

- Setting a directory ;
- Creating parca matrice (excel with cadastrals parcels info);
- Downloading parca geometry;
- Downloading data based on parca;
- GIS part to create analysis units ;
- Generating final layer from analysis units

### Setting a directory

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

### Creating parca matrice

`Rsequoia2` provides several ways to generate this fundamental file:

- A manually filled Excel file (see Cadastral Parcels article)
- Using legal-entity open data (see Cadastral Parcels - Legal Entity
  article)
- Generating the file from DGFiP PDF matrices

### Downloading parca geometry

All data downloaded by `Rsequoia2` are based on cadastral parcels
(parca). The first mandatory step is therefore to download them using
[`seq_parca()`](https://mucau.github.io/Rsequoia2/reference/seq_parca.md).

``` r
seq_parca()
```

### Downloading data

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
seq_mnhn()
```

### Analyst unist

The next step after downloading **parca** and the additional datasets is
to create the analysis units.

This layer is generated using
[`seq_parca_to_ua()`](https://mucau.github.io/Rsequoia2/reference/seq_parca_to_ua.md).
Your task is to segment the layer so that it reflects the actual forest
structure.

``` r
seq_parca_to_ua()
```

Additional layers can help with this step, but the real diagnostic comes
from field data collection.

For more details, see the Analysis units article.

### Finalizing the layer

The final step, after manually completing the UA layer, is to make sure
everything is correct (and, more importantly, that nothing was
accidently corrupted). Some additional processing is also required to
properly finalize this layer.

[`seq_ua()`](https://mucau.github.io/Rsequoia2/reference/seq_ua.md)
performs a series of checks and corrections to validate and finalize the
UA layer:

- Checks cadastral IDU consistency between UA and PARCA
- Ensures UA cadastral areas match PARCA by correcting any detected
  differences
- Generates a standardized management unit identifier (UG) in the UA
  object
- Computes corrected cadastral areas for analysis units based on
  cartographic area
- Checks management unit (UG) consistency within the UA sf object
- Detects and corrects minor inconsistencies within management units
  (UG)
- Cleans geometry topology

``` r
seq_ua()
```

### Summary

Last but not least, the final step is to generate tables that help users
better understand and write their forest management document.

[`seq_summary()`](https://mucau.github.io/Rsequoia2/reference/seq_summary.md)
produces a set of opinionated summary tables derived from the UA layer.

``` r
seq_summary()
```

## How it works

### Internal configuration

`Rsequoia2` relies on an internal configuration that maps keys to data
layers and their metadata. For example, the key `"r.sol.geol"` refers to
the geological raster layer named `GEOL.tif`.

All available keys can be listed with
[`seq_key()`](https://mucau.github.io/Rsequoia2/reference/seq_key.md).
To access metadata for a specific layer, use `seq_layer("r.sol.geol")`.

Note that full keys are not always required: `seq_layer("geol")` also
works.

### `seq_*` functions

All main functions in `Rsequoia2` start with `seq_*`
(e.g. [`seq_geol()`](https://mucau.github.io/Rsequoia2/reference/seq_geol.md),
[`seq_prsf()`](https://mucau.github.io/Rsequoia2/reference/seq_prsf.md),
…).

These functions: - Read the project cadastral parcels to defined area of
interest ; - Download the required data ; - Save them according to the
configuration (name and extension) ; - Return path(s) where the data are
stored.

Note: for advanced users, each `seq_*` function has an equivalent
`get_*` function (e.g. `get_geol()`,
[`get_prsf()`](https://mucau.github.io/Rsequoia2/reference/get_prsf.md),
…). These can be used independently of the Sequoia workflow.

### Cheat code !

For those who made it to the end, `Rsequoia2` provides a wrapper
function to guide you through the entire workflow:
[`sequoia()`](https://mucau.github.io/Rsequoia2/reference/sequoia.md).

Have fun !

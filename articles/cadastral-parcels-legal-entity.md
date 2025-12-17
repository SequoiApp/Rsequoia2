# Cadastral parcels - legal entity

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

As always, we need to setup a Sequoia dir (temporary in this case)

``` r
seq_dir <- file.path(tempdir(), "MY_FOREST")
dir.create(seq_dir)
```

## What is Legal Entity.

In France, legal entity is known as “Personne morale”. Unlike a physical
person, a legal entity has its own juridical existence and is identified
by national codes such as SIREN (9 digits) or SIRET. In Forestry,
typical entities include “groupements forestiers” or companies.

France provides open datasets that describe cadastral parcels owned by
legal entities. These files, published by the Direction Générale des
Finances Publiques (DGFiP), allow users to retrieve cadastral parcels
for a given owner.

`Rsequoia2` give acces to this dataset and helpers to search specific
owner.

### Retrieve legal entity owner

[`get_legal_entity()`](https://mucau.github.io/Rsequoia2/reference/get_legal_entity.md)
function download and prepare data as a Sequoia matrice.

There is a cache system to this function to avoid downloading each time.
By default `cache = NULL`whic downlaod dtata in a dedicated cache dir
(see [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

You should keep this directory but if for any reason you need raw data
elswhere use `cache`.

Be aware that you can load data from a department code or an insee code.
The first can be a bit time-consuming but you can then search all dep if
owner have cadastral parcels accros multiple commune.

``` r
insee <- c("29158", "29165")

legal_entity_cp <- get_legal_entity(insee)
#> ℹ Downloading legal entity datasets...
#> ⠙ 9 extracted | 430 MB (166 MB/s) | 2.6s
#> ⠹ 29 extracted | 1.3 GB (225 MB/s) | 5.6s
#> ⠙ 29 extracted | 1.1 GB (265 MB/s) | 4.1s
#> ⠹ 50 extracted | 2.0 GB (280 MB/s) | 7.1s
#> ✔ Data available at: /home/runner/.cache/R/Rsequoia2
#> ℹ Reading CSV files...
#> ℹ Preparing CSV files...
#> ℹ Generating matrice...
#> ✔ Matrix successfully generated (2329 rows).

head(legal_entity_cp)
#>   IDENTIFIANT         PROPRIETAIRE  IDU REG_NOM REG_CODE DEP_NOM DEP_NUM
#> 1                LES TERRES BLEUES <NA>    <NA>     <NA>    <NA>    <NA>
#> 2                LES TERRES BLEUES <NA>    <NA>     <NA>    <NA>    <NA>
#> 3                LES TERRES BLEUES <NA>    <NA>     <NA>    <NA>    <NA>
#> 4                LES TERRES BLEUES <NA>    <NA>     <NA>    <NA>    <NA>
#> 5             TI PORZH LES ROCHERS <NA>    <NA>     <NA>    <NA>    <NA>
#> 6                     L ATELIER 89 <NA>    <NA>     <NA>    <NA>    <NA>
#>   COM_NOM COM_CODE PREFIXE SECTION NUMERO              LIEU_DIT SURF_CA SOURCE
#> 1    <NA>     <NA>     000      AB   0005          DES GOELANDS      NA   <NA>
#> 2    <NA>     <NA>     000      AB   0006           DES EMBRUNS      NA   <NA>
#> 3    <NA>     <NA>     000      AB   0007         SAINT GUENOLE      NA   <NA>
#> 4    <NA>     <NA>     000      AB   0008          DES GOELANDS      NA   <NA>
#> 5    <NA>     <NA>     000      AB   0011         ROGER QUINIOU      NA   <NA>
#> 6    <NA>     <NA>     000      AB   0025 PIERRE ET JEAN DUPOUY      NA   <NA>
```

### Search legal entity owner

After loading the file you can use helpers
[`search_legal_entity()`](https://mucau.github.io/Rsequoia2/reference/search_legal_entity.md).

This helpers relies on a text–normalization applied to both the inputs
and the corresponding columns of the matrice. This makes the search
robust to accents, punctuation, spacing irregularities and case
differences.

Below we search all GFA (“Groupement Foncier Agricole”).

``` r
search_mat <- search_legal_entity(legal_entity_cp, prop = "gfa")

unique(search_mat$PROPRIETAIRE)
#> [1] "GFA DE LA TORCHE" "GFA ADPF"
```

### Save to start Sequoia process

As you can see in article …, starting a Sequoia process need a excel
matrice. To generate this one you can use
[`seq_xlsx()`](https://mucau.github.io/Rsequoia2/reference/seq_xlsx.md)
function.

Notice that the `data.frame` generated with
[`get_legal_entity()`](https://mucau.github.io/Rsequoia2/reference/get_legal_entity.md)
is already formated for Sequoia process. This mean you have the
`"IDENTIFIANT"` columns. To avoid filling it in excel, we encouraged you
to do it before saving.

``` r
id <- "MY_FOREST"
search_mat$IDENTIFIANT <- id

# "MATRICE" = search_mat mean write search_mat to sheet "MATRICE
seq_xlsx(
  x = list("MATRICE" = search_mat),
  filename = file.path(seq_dir, paste0(id, "_matrice.xlsx"))
)
#> ✔ Excel file created at: /tmp/RtmpCmqnrE/MY_FOREST/MY_FOREST_matrice.xlsx
```

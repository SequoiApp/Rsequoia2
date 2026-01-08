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
#> Attempting to load the package cache ... No cache found.
#> Will try to reconfigure qgisprocess and build new cache ...
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
#> ⠙ 11 extracted | 482 MB (166 MB/s) | 2.9s
#> ⠹ 17 extracted | 773 MB (131 MB/s) | 5.9s
#> ⠙ 15 extracted | 544 MB (232 MB/s) | 2.3s
#> ⠹ 17 extracted | 631 MB ( 100 MB/s) | 6.3s
#> ⠸ 23 extracted | 841 MB (100 MB/s) | 8.4s
#> ⠼ 42 extracted | 1.7 GB (150 MB/s) | 11.3s
#> ✔ Data available at: /home/runner/.cache/R/Rsequoia2
#> ℹ Reading CSV files...
#> ℹ Preparing CSV files...
#> ℹ Generating matrice...
#> ✔ Matrix successfully generated (2329 rows).

head(legal_entity_cp)
#>   IDENTIFIANT         PROPRIETAIRE            IDU  REG_NOM REG_CODE   DEP_NOM
#> 1        <NA>    LES TERRES BLEUES 29158000AB0005 BRETAGNE       53 FINISTERE
#> 2        <NA>    LES TERRES BLEUES 29158000AB0006 BRETAGNE       53 FINISTERE
#> 3        <NA>    LES TERRES BLEUES 29158000AB0007 BRETAGNE       53 FINISTERE
#> 4        <NA>    LES TERRES BLEUES 29158000AB0008 BRETAGNE       53 FINISTERE
#> 5        <NA> TI PORZH LES ROCHERS 29158000AB0011 BRETAGNE       53 FINISTERE
#> 6        <NA>         L ATELIER 89 29158000AB0025 BRETAGNE       53 FINISTERE
#>   DEP_NUM  COM_NOM COM_CODE INSEE PREFIXE SECTION NUMERO              LIEU_DIT
#> 1      29 PENMARCH       91 29158     000      AB   0005          DES GOELANDS
#> 2      29 PENMARCH       91 29158     000      AB   0006           DES EMBRUNS
#> 3      29 PENMARCH       91 29158     000      AB   0007         SAINT GUENOLE
#> 4      29 PENMARCH       91 29158     000      AB   0008          DES GOELANDS
#> 5      29 PENMARCH       91 29158     000      AB   0011         ROGER QUINIOU
#> 6      29 PENMARCH       91 29158     000      AB   0025 PIERRE ET JEAN DUPOUY
#>   SURF_CA
#> 1     370
#> 2   12358
#> 3      97
#> 4     765
#> 5     339
#> 6     237
#>                                                                                                             SOURCE
#> 1 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
#> 2 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
#> 3 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
#> 4 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
#> 5 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
#> 6 https://data.economie.gouv.fr/api/v2/catalog/datasets/fichiers-des-locaux-et-des-parcelles-des-personnes-morales
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
#> ✔ Excel file created at: /tmp/RtmpYexpCa/MY_FOREST/MY_FOREST_matrice.xlsx
```

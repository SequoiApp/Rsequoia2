# Sequoia (FR)

``` r
knitr::opts_chunk$set(
  fig.width = 7.291667,
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  out.width = "100%"
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
library(tmap)
tmap_options(scale = 0.75)
```

## Sequoia

### Qu’est-ce que Sequoia

Sequoia est une approche de la gestion forestière, reposant sur des
choix assumés afin de guider l’utilisateur vers des pratiques cohérentes
et reproductibles. Son objectif est de

- Faciliter le téléchargement des données nécessaires à la gestion
  forestière ;
- Normaliser les données (nommage et structure) ;
- Faciliter l’exploration et l’analyse de ces données.

### Qu’est-ce que le processus Sequoia

Un processus Sequoia comprend toujours les étapes suivantes :

- Définition d’un répertoire de travail ;
- Création d’une matrice cadastrales ;
- Téléchargement des parcelles cadastrales ;
- Téléchargement des données utiles à la gestion forestière ;
- Analyse SIG pour créer des unités d’analyse ;
- Génération des couches de gestion à partir des unités d’analyse.

Pour vous guider a travers ces différentes étapes, `Rsequoia2` propose
la fonction
[`sequoia()`](https://sequoiapp.github.io/Rsequoia2/reference/sequoia.md).
Cette dernière ouvre un menu dans la console pour faciliter l’accès aux
fonctionnalités.

``` r
sequoia()
```

Amusez-vous bien ! \### Définir un répertoire

Toutes les fonctions principale de `Rsequoia2` disposent de l’argument
`dirname = "."`, ce qui signifie qu’elles utilisent par défaut le
répertoire de travail courant. Il est recommandé de créer un répertoire
dédié et de le définir comme répertoire de travail de la session R.

Dans cet exemple, une forêt nommée `"ECKMUHL"` est utilisée :

``` r
sequoia_dir <- "ECKMUHL"
dir.create(sequoia_dir)

# définir le répertoire de travail
setwd(sequoia_dir)
```

#### Créer la matrice cadastrale

`Rsequoia2` propose plusieurs méthodes pour générer ce fichier :

- Création d’une matrice vierge complétée manuellement (voir l’article
  Parcelles cadastrales);
- Recherche des parcelles pour les personne morale et génération
  automatique de la matrice (voir l’article Parcelles cadastrales -
  Personnes morales);
- Convertion des matrices PDF de la DGFiP en fichier excel

#### Télécharger la géométrie des parcelles cadastrales

Toutes les données téléchargées par `Rsequoia2` reposent sur les
parcelles cadastrales (couche parca). La première étape obligatoire
consiste donc à les télécharger avec :

``` r
seq_parca()
```

#### Télécharger les données

`Rsequoia2` propose de nombreux jeux de données, téléchargeables via les
fonctions `seq_*` :

- Données environnementales du MNHN
  ([`seq_mnhn()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_mnhn.md));
- Données altimétriques de l’IGN
  ([`seq_elevation()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_elevation.md));
- Orthophotographies de l’IGN
  ([`seq_ortho()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_ortho.md));
- Données géologiques du BRGM
  ([`seq_geol()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_geol.md));
- Données hydrologiques de la BD Topo IGN
  ([`seq_hydro()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_hydro.md));
- Données patrimoniales de l’Atlas du Patrimoine
  ([`seq_patrimony()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_patrimony.md));
- …

Exemple :

``` r
seq_mnhn()
```

#### Unités d’analyse

L’étape suivante, après le téléchargement du parca et des jeux de
données complémentaires, consiste à créer les unités d’analyse.

Cette couche est générée avec :

``` r
seq_parca_to_ua()
```

La couche UA doit alors être travailer sur un logiciel SIG pour qu’elle
reflète fidèlement la structure forestière réelle.

Ce travail est souvent réalisé en deux étapes : - Une
photo-interpétation ; - Une phase terrain pour préciser

Pour plus de détails, consultez l’article sur les unités d’analyse.

#### Finalisation de la couche

Une fois que la couche UA est complétée, il est nécessaire de la valider
:

``` r
seq_ua()
```

Cette fonction effectue une série de vérifications et de corrections :

- Elle vérifie la cohérence des identifiants cadastraux (IDU) entre UA
  et PARCA ;
- Elle assure la correspondance des surfaces cadastrales entre UA et
  PARCA en corrigeant les écarts éventuels ;
- Elle génère un identifiant standardisé pour chaque unité de gestion
  (ensemble d’UA cohérentes entre-elles);
- Elle calcule les surfaces cadastrales corrigées à partir des surfaces
  cartographiques ;
- Elle vérifie la cohérence des unités de gestion ;
- Elle détecte et corrige les incohérences mineures ;
- Elle nettoie la topologie des géométries.

#### Synthèse

La dernière étape consiste à générer des tableaux de synthèse facilitant
l’analyse et la rédaction du document de gestion forestière :

``` r
seq_summary()
```

Cette fonction produit un ensemble de tableaux de synthèse standardisés
à partir de la couche UA.

### Fonctionnement interne

#### Configuration interne

`Rsequoia2` repose sur une configuration interne qui associe des clés
aux couches de données et à leurs métadonnées.

Par exemple, la clé `"r.sol.geol"` correspond à la couche raster
géologique “GEOL.tif”.

Toutes les clés disponibles peuvent être listées avec :

``` r
seq_key()
```

Pour accéder aux métadonnées d’une couche spécifique :

``` r
seq_layer("r.sol.geol")
```

Les clés partielles sont également acceptées :

``` r
seq_layer("geol")
```

#### Fonctions `seq_*`

Toutes les fonctions principales commencent par `seq_*` (ex :
[`seq_geol()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_geol.md),
[`seq_prsf()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_prsf.md),
…).

Ces fonctions ont toutes le même fonctionnement :

- Elles lisent les parcelles cadastrales du projet pour définir
  l’emprise d’étude ;
- Elles téléchargent les données nécessaires ;
- Elles les enregistrent selon la configuration standardisée (nom et
  extension) ;
- Elles renvoient le ou les chemins vers les fichiers générés.

Pour les utilisateurs avancés, chaque fonction `seq_*` possède un
équivalent `get_*` (ex : `get_geol()`,
[`get_prsf()`](https://sequoiapp.github.io/Rsequoia2/reference/get_prsf.md),
…) utilisable indépendamment du workflow Sequoia.

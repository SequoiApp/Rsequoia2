# Unités d'analyse

``` r

library(Rsequoia2)
library(tmap)
library(openxlsx2)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
```

## Introduction

### De quoi parle-t-on ?

La gestion forestière (aménagement forestier) consiste à planifier les
interventions en forêt :

- les opérations de coupe, qui génèrent des revenus ;

- les travaux sylvicoles, qui correspondent à des investissements.

Pour établir cette planification, un système de parcellaire de gestion
(c’est-à-dire des unités de gestion) est nécessaire.

Une unité de gestion correspond à une zone homogène du point de vue
forestier. Cette unité dispose donc d’une superficie définie.

Le contexte forestier français implique un ensemble de **principes de
bon sens**, tandis que l’administration française impose quelques
**règles fondamentales**.

Parmi les règles fondamentales, rappelons que **l’on travaille avec des
surfaces cadastrales** et non des surfaces cartographiques.

Ce package propose une approche intégrative.

### Zone d’étude utilisée

La forêt étudiée dans l’article sera la forêt de Brin, propriété de
l’école forestière de Nancy, située à Brin-Sur-Seille, dans le
département de Meurthe-Et-Moselle (54). Les données présentées sont
fictives et ne reflètent pas du tout la description ou la gestion de la
forêt.

------------------------------------------------------------------------

## 1. Définitions Sequoia

### 1.1. Unité d’analyse

Une **unité d’analyse** (*ua*) correspond à la plus petite fraction
d’une parcelle cadastrale qui est **homogène du point de vue de ses
attributs de descriptition forestière**, et est désignée par un
identifiant unique d’unité de gestion (*ug*).

En d’autre termes, imaginez le cadastre d’une propriété. Superposez lui
vous sous-parcelles de gestion et vous obtenez la couche *UA*.

Autrement dit, chaque *ua* :

- conserve ses attributs cadastraux afin de pouvoir calculer les
  surfaces cadastrales : l’identifiant cadastral unique (IDU) et la
  surface cadastrale (SURF_CAD) ;

- reçoit une description forestière via le remplissage de la table
  attributaire : type de peuplement (PLT_PLMT), richesse du peuplement
  (PLT_RICH), stade du peuplement (PLT_RICH) et année d’installation du
  peuplement (PLT_ANNE) au minimum ;

- reçoit un identifiant d’unité de gestion forestière composé d’un
  numéro de parcelle forestière (N_PARFOR) et d’un numéro de
  sous-parcelle (N_SSPARFOR).

### 1.2. Unité de gestion

Une **unité de gestion** (*ug*) correspond à une zone homogène du point
de vue de sa description forestière et qui est gérée sous **un
traitement sylvicole unique**. Elle est composée de plusieurs unités
d’analyse.

Autrement dit, chaque *ua* d’une *ug* doit avoir :

- la même description forestière (même point de départ) ;

- la même destination forestière (même point d’arrivée).

L’*ug* dispose d’un identifiant correspondant à l’aggrégation du code de
parcelle et du code de la sous-parcelle.

### 1.3. Parcelles forestières

Les **parcelles forestières** (*pf*) correspondent à **un découpage
arbitraire** d’une forêt, décidé par le propriétaire ou son gestionnaire
forestier.

***Recommandations :***

- Si des parcelles forestières existent, vous devez travailler avec.

- Si des parcelles forestières existent mais ne sont pas exploitables,
  corrigez-les en les rattachant aux éléments structurants du massif
  (routes, rivières, talus, pistes).

- Si des parcelles forestières n’existent pas, créez-les en les
  rattachant aux éléments structurants. Ne pas utiliser les peuplements,
  car la forêt peut évoluer rapidement.

### 1.4. Sous-parcelles forestières

Les **sous-parcelles** (*sspf*) correspondent à la subdivision dans la
parcelle forestière des unités de gestions *UGs*. En clair, 1 *sspf* = 1
*ug*.

------------------------------------------------------------------------

## 2. Obtenir une couche *PARCA* valide

Comme indiqué précédement, la couche *UA* doit contenir les attributs
cadastraux. C’est pourquoi, la couche *UA* est créée à partir de la
couche des parcelles cadastrales *PARCA*.

Suivez les intructions de la rubrique dédié au [parcellaire
cadastral](https://sequoiapp.github.io/Rsequoia2/articles/02_cadastral-parcels.html).

Pour être valide, la topologie de la couche *PARCA* doit avoir été
vérifiée !

### 2.1. Fonction “base” (rappel)

Pour rappel, il suffit d’utiliser la fonction
[`get_parca()`](https://sequoiapp.github.io/Rsequoia2/reference/get_parca.md)
en saisissant des **IDUs** cadastraux.

Par gain de temps, pour l’exemple de BRIN, nous allons sauter cette
étape déjà décrite dans l’article précédent et utiliser le dataset du
package.

``` r

# Exemple pour la forêt de BRIN
brin_parca <- Rsequoia2::brin_parca
brin_foret <- Rsequoia2::seq_dissolve(brin_parca, 5.5)

tm_shape(brin_parca)+
  tm_borders(col = "blue", lwd = 2)+
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 2.2. Fonction “process” (rappel)

Pour rappel, il suffit d’utiliser la fonction
[`seq_parca()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_parca.md)
en indiquant le dossier.

``` r


# Project folder creation
eckmuhl_matrice <- read_xlsx(system.file("extdata/ECKMUHL_matrice.xlsx", package = "Rsequoia2"))
eckmuhl_sequoia_dir <- file.path(tempdir(), "ECKMUHL")
dir.create(eckmuhl_sequoia_dir)
write_xlsx(eckmuhl_matrice, file.path(eckmuhl_sequoia_dir, "ECKMUHL_matrice.xlsx"))

# PARCA creation
eckmuhl_parca_path <- seq_parca(eckmuhl_sequoia_dir)
#> ℹ Downloading BDP from IGN...
#> ✔ 9 of 9 ETALAB geom successfully replaced with BDP geom.
#> ✔ No area inconsistencies (cadastre vs GIS) detected.
#> ✔ Layer "v.seq.parca.poly" with 9 features saved to 1_SEQUOIA/ECKMUHL_SEQ_PARCA_poly.gpkg.
#> ✔ Table "x.seq.matrice" saved to ECKMUHL_MATRICE.xlsx.
#> ✔ _matrice.xlsx also saved as ECKMUHL_matrice_20260527T083455.xlsx for safety.
eckmuhl_parca <- read_sf(eckmuhl_parca_path)
eckmuhl_foret <- Rsequoia2::seq_dissolve(eckmuhl_parca, 5.5)

# Plot
tm_shape(eckmuhl_parca)+
  tm_borders(col = "blue", lwd = 2)+
tm_shape(eckmuhl_foret)+
  tm_borders(col = "black", lwd = 3)
```

------------------------------------------------------------------------

## 3. Obtenir la couche *UA*

### 3.1. Fonction “base”

La fonction
[`parca_to_ua()`](https://sequoiapp.github.io/Rsequoia2/reference/parca_to_ua.md)
permet de générer la couche *UA*.

Notez qu’à sa création, la couche des unités d’analyse n’est rien
d’autre que la couche des parcelles cadastrales *PARCA* enrichie de
champs d’attributs supplémentaires.

``` r


brin_ua_raw <- parca_to_ua(brin_parca)

# PARCA structure
nrow(brin_parca)
#> [1] 114
ncol(brin_parca)
#> [1] 18

# UA structure
nrow(brin_ua_raw) # same as PARCA
#> [1] 114
ncol(brin_ua_raw)
#> [1] 45

# Plot
tm_shape(brin_parca)+
  tm_borders(col = "blue", lwd = 2)+
tm_shape(brin_ua_raw)+
  tm_borders(col = "orange", lwd = 1)+
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 3.2. Fonction “process”

Si l’utilisateur opte pour le procesus *Sequoia*, la fonction
[`seq_parca_to_ua()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_parca_to_ua.md)
travaille directement avec le dossier normalisé en récupérant la couche
*PARCA*.

La couche *UA* est directement écrite dans le dossier.

``` r

# UA creation
eckmuhl_ua_path <- seq_parca_to_ua(eckmuhl_sequoia_dir)
#> ✔ Loaded vector layer "v.seq.parca.poly" from ECKMUHL_SEQ_PARCA_poly.gpkg.
#> ✔ Layer "v.seq.ua.poly" with 9 features saved to 1_SEQUOIA/ECKMUHL_SEQ_UA_poly.gpkg.
eckmuhl_ua <- read_sf(eckmuhl_ua_path)

# Plot
tm_shape(eckmuhl_parca)+
  tm_borders(col = "blue", lwd = 2)+
tm_shape(eckmuhl_ua)+
  tm_borders(col = "orange", lwd = 1)+
tm_shape(eckmuhl_foret)+
  tm_borders(col = "black", lwd = 3)
```

------------------------------------------------------------------------

## 4. Travailler avec la couche *UA*

### 4.1. Fragmentation

Notre approche consiste à diviser les parcelles cadastrales en fractions
(les unités d’analyse), selon des étapes logiques.

Cette segmentation ne peut être réalisée qu’à l’aide d’un logiciel de
SIG. Nous recommandons vivement la suite [Quntum
GIS](https://sequoiapp.github.io/Rsequoia2/articles/%60r%20qgis%60).

Comme étapes de fragmentation (découpage), nous recommandons :

1.  Commencer par les emprises des routes ;
2.  Poursuivre avec les limites des parcelles forestières ;
3.  Continuer avec les pistes importantes ;
4.  Enfin, distinguer les peuplements.

À chaque étape, l’utilisateur est tenu de compléter la table des champs
d’attributs.

### 4.2. Champs attributaires

La couche *UA* est riche en attribut, afin de s’adapter à une large
gamme d’organisation et de méthode de travail.

| Champ | Thématique | Description | Commentaire |
|----|----|----|----|
| “IDENTIFIANT” | Projet | Identifiant du projet | Hérité de *PARCA* |
| “PROPRIETAIRE” | PARCA | Propriétaire | Hérité de *PARCA* |
| “IDU” | PARCA | Identifiant cadastral de la *parca* | Hérité de *PARCA* |
| “REG_NOM” | PARCA | Nom de région | Hérité de *PARCA* |
| “REG_CODE” | PARCA | Code de région | Hérité de *PARCA* |
| “DEP_NOM” | PARCA | Nom de département | Hérité de *PARCA* |
| “DEP_CODE” | PARCA | Code de département | Hérité de *PARCA* |
| “COM_NOM” | PARCA | Nom de commune | Hérité de *PARCA* |
| “COM_CODE” | PARCA | Code de commune | Hérité de *PARCA* |
| “INSEE” | PARCA | Code INSEE de commune | Hérité de *PARCA* |
| “PREFIXE” | PARCA | Préfixe de la *parca* | Hérité de *PARCA* |
| “SECTION” | PARCA | Section de la *parca* | Hérité de *PARCA* |
| “NUMERO” | PARCA | Numéro de la *parca* | Hérité de *PARCA* |
| “LIEU_DIT” | PARCA | Lieu-dit de la *parca* | Hérité de *PARCA* |
| “BOISE” |  | Nature de culture de l’\*ua’ | Permet de discriminer les surfaces boisées et non boisées |
| “PARFOR” | Localisant | Code de l’*ug* | Aggrégation `N_PARFOR` & `N_SSPARFOR` |
| “N_PARFOR” | Localisant | Parcelle de l’*ua* |  |
| “N_SSPARFOR” | Localisant | Sous-parcelle de l’*ua* |  |
| “PLT_PLMT” | Peuplement | Type de peuplement l’*ua* | classification de l’utilisateur |
| “PLT_RICH” | Peuplement | Richesse de peuplement l’*ua* | classification de l’utilisateur |
| “PLT_STAD” | Peuplement | Stade de peuplement l’*ua* | classification de l’utilisateur |
| “PLT_ANNE” | Peuplement | Année d’origine du peuplement l’*ua* | année |
| “SINISTRE” | Peuplement | Le peuplement l’*ua* est-il sinistré ? | booléan TRUE/FALSE |
| “DISPONIBLE” | Peuplement | Le peuplement l’*ua* est-il disponible (en production) ? | booléan TRUE/FALSE |
| “CLOISONNE” | Peuplement | Le peuplement l’*ua* est-il closionné ? | booléan TRUE/FALSE |
| “RES_ESS1” | Futaie | Essence principale de la futaie dans l’*ua* | 1 essence |
| RES_ESSS” | Futaie | Essences secondaires de la futaie dans l’*ua* | Plusieurs essences |
| “RES_STRUCTURE” | Futaie | Structure de la futaie dans l’*ua* | classification de l’utilisateur |
| “TSE_ESS1” | Taillis | Essence principale du taillis dans l’*ua* | 1 essence |
| “TSE_ESSS” | Taillis | Essences secondaires du taillis dans l’*ua* | Plusieurs essences |
| “TSE_DENSITE” | Taillis | Densité du taillis dans l’*ua* | classification de l’utilisateur |
| “TSE_NATURE” | Taillis | Nature du taillis dans l’*ua* | classification de l’utilisateur |
| “JEU_ESS1” | Jeunesse | Essence principale de la jeunesse (régénération) dans l’*ua* | 1 essence |
| “JEU_ESSS” | Jeunesse | Essences secondaires de la jeunesse (régénération) dans l’*ua* | Plusieurs essences |
| “JEU_STADE” | Jeunesse | Stade de développement de la jeunesse (régénération) dans l’*ua* | classification de l’utilisateur |
| “JEU_DENSITE” | Jeunesse | Densité de la jeunesse (régénération) dans l’*ua* | classification de l’utilisateur |
| “AME_TYPE” | Gestion | Traitement sylvicole retenue pour l’*ug* | classification de l’utilisateur |
| “SUBVENTIONNE” | Subvention | Les interventions dans l’*ug* font-elles l’objet de subventions ? | booléan TRUE/FALSE |
| “SUB_DISPOSITIF” | Subvention | Dispositif de subvention de l’*ug* | 1 dispositif |
| “COMMENTS” |  | Commentaire dans l’*ua* | libre |
| “STATION” |  | Station dans l’*ua* | libre |
| “SURF_SIG” | Surface | Surface cartographiqe de l’*ua* |  |
| “SURF_COR” | Surface | Surface cadastral corrigée de l’*ua* |  |
| “SURF_CAD” | Surface | Surface cadastral de la *parca* | Hérité de *PARCA* |

Un exemple de table fragmentée et complété vous est proposé !

``` r

brin_ua <- Rsequoia2::brin_ua
View(brin_ua)
```

------------------------------------------------------------------------

## 5. Obtenir la surface cadastrale

Lorsque vous avez fini de fragmenter la couche *UA*, vous pouvez valider
la couche.

**/!\\** La couche des *UA* doit être fermée dans votre logiciel de SIG
avant l’exécution !

### 5.0. Avant, vérifier la topologie

Assurez-vous que la couche ne contient aucune erreur topologique en la
vérifiant depuis votre logiciel de SIG.

### 5.1. Fonction “base”

La fonction
[`ua_to_ua()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_to_ua.md)
permet de valider la couche.

Cette fonction écrase la couche des *UA* : c’est la même couche, mais
avec :

- les **IDU** vérifiés par rapport au *PARCA* ;
- les champs cadastraux vérifiés et corrigés par rapport au *PARCA* ;
- les champs de l’*ug* générés ;
- les surfaces cadastrales vérifiées et corrigées ;
- les surfaces cadastrales corrigées calculées ;
- la cohérence des unités de gestion vérifiée et éventuellement
  corrigée.

Notez que la cohérence des *UGs* est assurée de telle sorte que la
description incohérente des petites *ua* d’une même *ug* sont corrigés à
partir de la description dominante de celle-ci.

``` r

brin_ua<- ua_to_ua(brin_ua, brin_parca)
#> ✔ All UA IDU values are already correct.
#> ✔ UA and PARCA IDU values are consistent.
#> ✔ UG field PARFOR created.
#> ✔ Corrected cadastral areas calculated.
#> ✔ All UG are consistent.

# Plot
tm_shape(brin_parca)+
  tm_borders(col = "blue", lwd = 2)+
tm_shape(brin_ua)+
  tm_borders(col = "orange", lwd = 1)+
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 5.2. Fonction “process”

Il vous suffit d’éxécuter la fonction
[`seq_ua()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_ua.md)
qui récupère directement la couche dans le répertoire du projet.

``` r


eckmuhl_ua_path <- seq_ua(eckmuhl_sequoia_dir)
eckmuhl_ua <- read_sf(eckmuhl_ua_path)

tm_shape(eckmuhl_ua)+
  tm_borders(col = "firebrick", lwd = 2)
```

------------------------------------------------------------------------

## 5. Utilisation de la couche *UA* actualisée

Etant donnée la richesse de la couche *UA*, de nombreuses analyses
peuvent être réalisées dessus.

Quelques exemples de représentation duplicable dans votre SIG:

### 5.1. Type de peuplement

``` r

tm_shape(brin_ua) +
  tm_polygons(
    fill = "PLT_PLMT",
    fill.scale = tm_scale_categorical(values = c(
      BAT = "white",
      BET = "black",
      EAU = "cyan",
      EMD = "white",
      FRF = "darkgreen",
      FRR = "tomato",
      LAH = "grey",
      MFT = "green",
      REF = "lightgreen",
      TSN = "brown"
    )),
    fill_alpha = 0.7
  ) +
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 5.1. Richesse de peuplement

``` r

tm_shape(brin_ua) +
  tm_polygons(
    fill = "PLT_RICH",
    fill.scale = tm_scale_categorical(values = c(
      RAS = "white",
      TPA = "khaki",
      PPA = "lightgreen",
      MRI = "forestgreen",
      RRI = "darkgreen"
    )),
    fill_alpha = 0.7
  ) +
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 5.3. Stade de peuplement

``` r

tm_shape(brin_ua) +
  tm_polygons(
    fill = "PLT_STAD",
    fill.scale = tm_scale_categorical(values = c(
      RAS = "white",
      SFO = "powderblue",
      JEU = "royalblue",
      ADU = "blue",
      RSF = "navyblue",
      EXP = "white"
    )),
    fill_alpha = 0.7
  ) +
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 5.4. Peuplements complets

``` r

brin_ua$combinaison <- paste(brin_ua$PLT_PLMT, 
                             brin_ua$PLT_RICH, 
                             brin_ua$PLT_STAD, 
                             sep = "_")

tm_shape(brin_ua) +
  tm_polygons(
    fill = "combinaison",
    fill.scale = tm_scale_categorical(values = c(
      BAT_RAS_RAS = "white",
      BET_RAS_RAS = "black",
      EAU_RAS_RAS = "cyan",
      EMD_RAS_RAS = "white",
      REF_RAS_SFO = "powderblue",
      FRF_MRI_JEU = "royalblue",
      FRF_RRI_ADU = "navyblue",
      FRR_MRI_ADU = "tomato",
      LAH_RAS_RAS = "grey",
      MFT_TPA_RSF = "khaki",
      MFT_TPA_ADU = "khaki",
      MFT_PPA_RSF = "lightgreen",
      MFT_PPA_ADU = "lightgreen",
      MFT_MRI_ADU = "forestgreen",
      TSN_RAS_EXP = "brown"
    )),
    fill_alpha = 0.7
  ) +
tm_shape(brin_foret) +
  tm_borders(col = "black", lwd = 3)
```

------------------------------------------------------------------------

## 6. Obtenir les produits de l’*UA* actualisée

Lorsque vous avez mis à jour la couche des *UA*, vous pouvez obtenir de
nouvelles couches :

- **PF (Parcelle)** : Les *UAs* sont regroupées en utilisant le champ
  `parcelle`, et les surfaces corrigées (`cor_area`) sont additionnées.
  Une couche de polygones et une couche de lignes de limite sont créées.

- **SSPF (Sous-parcelle)** : Les *UAs* sont regroupées en utilisant les
  champs N_PARFOR et N_SSPARFOR, et les surfaces corrigées (`cor_area`)
  sont additionnées. Une couche de polygones et une couche de lignes de
  limite sont créées.

### 6.1. Fonction “base”

Il n’existe pas une unique fonction, mais plusieurs: - la fonction
[`ua_to_pf()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_to_pf.md)
permet d’obtenir le parcellaire forestier ; - la fonction
[`ua_to_sspf()`](https://sequoiapp.github.io/Rsequoia2/reference/ua_to_sspf.md)
permet d’obtenir le sous-parcellaire forestier ; - la fonction
[`poly_to_line()`](https://sequoiapp.github.io/Rsequoia2/reference/poly_to_line.md)
permet d’obtenir les bordures.

``` r

# pf
brin_pf_poly <- ua_to_pf(brin_ua)
brin_pf_line <- poly_to_line(brin_pf_poly)

# sspf
brin_sspf_poly <- ua_to_sspf(brin_ua)
brin_sspf_line <- poly_to_line(brin_sspf_poly)

# Plot
tm_shape(brin_sspf_poly)+
  tm_borders(col = "green", lwd = 2)+
tm_shape(brin_pf_poly)+
  tm_borders(col = "darkgreen", lwd = 1)+
  tm_text("N_PARFOR")+
tm_shape(brin_foret)+
  tm_borders(col = "black", lwd = 3)
```

### 6.2. Fonction “process”

Lancer simplement
[`seq_parcels()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_parcels.md).

``` r

# generate new products by running
eckmuhl_paths <- seq_parcels(eckmuhl_sequoia_dir)

# read new product
eckmuhl_pf_poly   <- st_read(eckmuhl_paths[[1]], quiet = TRUE)
eckmuhl_pf_line   <- st_read(eckmuhl_paths[[2]], quiet = TRUE)
eckmuhl_sspf_poly <- st_read(eckmuhl_paths[[3]], quiet = TRUE)
eckmuhl_sspf_line <- st_read(eckmuhl_paths[[4]], quiet = TRUE)

# plot
tm_shape(eckmuhl_pf_line) +
  tm_lines(col = "blue", lwd = 3) +
tm_shape(eckmuhl_pf_poly) +
  tm_text("N_PARFOR", col = "blue", size = 0.8) +
tm_shape(eckmuhl_sspf_line) +
  tm_lines(col = "black", lwd = 1) +
tm_shape(eckmuhl_sspf_poly) +
  tm_text("PARFOR", col = "black", size = 0.7)
```

------------------------------------------------------------------------

## 7. Obtenir la synthèse Excel

### 7.2. Fonction “process”

La fonction
[`seq_summary()`](https://sequoiapp.github.io/Rsequoia2/reference/seq_summary.md)
permet de générer la synthèse Excel dans le répertoire.

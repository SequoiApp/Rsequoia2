# Analysis units

## What are we looking for?

Forest management (forest planning) consists in scheduling forest
interventions:

- harvesting operations that generate revenue;

- silvicultural works that require investments.

To establish this schedule, a management parcel system (i.e. management
units) is required.

A management unit corresponds to an area that is homogeneous from a
forestry perspective and must therefore have a defined surface area.

The French forestry context implies a set of **common-sense
principles**, while the French administration imposes some **fundamental
rules**.

Among the fundamental rules, let us remember that **we work with
cadastral surfaces** and not cartographic surfaces.

This package offers an integrative approach.

## 0. Sequoia definitions

### Analysis unit

An **analysis unit** (AU) corresponds to the smallest fraction of a
cadastral parcel that is **homogeneous in terms of its descriptive
forest attributes**, and is designated by a unique management unit (MU)
identifier.

In other words, each *AU* :

- keep its cadastral attributes in order to calculate cadastral areas:
  unique cadastral identifier (IDU) and cadastral surface (SURF_CAD);

- receives a forest description by filling the attribute table: stand
  type (PLT_PLMT), stand wealth (PLT_RICH), stand stage (PLT_RICH) and
  year of stand establishment (PLT_ANNE) at least;

- receives a forest management unit identifier composed by a forest
  parcel number (N_PARFOR) and a sub-parcel number (N_SSPARFOR).

### Management unit

An **management unit** (MU) corresponds to an area that is homogeneous
in terms of its forest description and that is managed under **a single
silvicultural regime**. It is composed of several analysis units.

In other words, each *AU* of a *MU* must have :

- the same forest description (same starting point);

- the same forestry destiny (same point of arrival).

### Forest parcels

**Forest parcels** (FP) division corresponds to **an arbitrary
subdivision** of a forest, decided by owner or last forest manager.

***Rules to follow:***

- If forest parcels exist, you have to work with.

- If forest parcels exist, but are not usable, correct them by attaching
  them to the structural elements of the massif (roads, rivers,
  embankment, tracks).

- If forest parcels doesn’t exist, create them by attaching them to the
  structural elements. Do not use stands because forest can be evolve
  quickly.

## 1. How do I create the analysis unit layer?

As mentioned before, the *AU* layer must keep cadastral attribute.
That’s why the *AU* layer is created from the cadastral parcels layers
*CP*.

### Before creation, check topology

…

### Run `seq_parca_to_ua()` and load the layer

At the time of its creation, the analysis unit layer is nothing more
than the cadastral parcel layer enriched with additional attribute
fields.

``` r
# remove eval = FALSE when data is available 

ua_path <- seq_parca_to_ua(sequoia_dir)
ua <- read_sf(ua_path)

tm_tiles("OpenStreetMap")+
tm_shape(ua)+
  tm_borders(col = "firebrick", lwd = 2)
```

## 2. How do I work with the analysis unit layer?

Our approach consists of dividing cadastral parcels into fractions (the
units of analysis), according to logical steps.

This segmentation can only be performed using GIS software. We strongly
recommend the [Quntum GIS
suite](https://mucau.github.io/Rsequoia2/articles/%60r%20qgis%60).

As segmentation steps, we recommend :

1.  Start with roads right-of-way;
2.  Continue with forest parcels limits;
3.  Continue with important tracks;
4.  Finally, distinguish the stand.

At each step, the user is required to complete the attribute fields
table.

## 3. How do I obtains the cadastral area?

When you have finish to segmentize the *AU* layer, you can get validate
the *UA* layer by running
[`seq_ua()`](https://mucau.github.io/Rsequoia2/reference/seq_ua.md).

**/!\\** *The AU* layer must be closed on your GIS software before
running !

``` r
# remove eval = FALSE when data is available

ua_path <- seq_ua(sequoia_dir)
ua <- read_sf(ua_path)

tm_tiles("OpenStreetMap")+
tm_shape(ua)+
  tm_borders(col = "firebrick", lwd = 2)
```

This function overwrite the *AU* layer: it’s the same layer, but with:

- IDUs checked against PARCA;

- cadastral areas checked and corrected;

- management unit fields generated; - corrected cadastral areas added;

- management unit consistency checked and corrected.

## 4. How can I use the updated *AU* layer after ?

### Before, check topology

Ensure that the layer does not contain any topological errors by
checking it from your GIS software.

### Run `seq_parcels()` and load layers

When you have updated the *AU* layer, you can get new products:

- **PF (Parcelle)**: Rows of the *AU* are grouped using the `parcelle`
  field, and corrected surface areas (`cor_area`) are summed. A polygon
  and border lines layer are created.

- **SSPF (Sous-parcelle)**: Rows of the *AU* are grouped using the
  N_PARFOR and N_SSPARFOR field, and corrected surface areas
  (`cor_area`) are summed. A polygon and border lines layer are created.

``` r
# remove eval = FALSE when data is available 

# generate new products by running
paths <- seq_parcels(sequoia_dir)

# read new product
pf_poly   <- st_read(paths[[1]], quiet = TRUE)
pf_line   <- st_read(paths[[2]], quiet = TRUE)
sspf_poly <- st_read(paths[[3]], quiet = TRUE)
sspf_line <- st_read(paths[[4]], quiet = TRUE)

# view
tmap_mode("view")

tm_shape(pf_line) +
  tm_lines(col = "blue", lwd = 3) +

tm_shape(pf_poly) +
  tm_text("N_PARFOR", col = "blue", size = 0.8) +

tm_shape(sspf_line) +
  tm_lines(col = "black", lwd = 1) +

tm_shape(sspf_poly) +
  tm_text("PARFOR", col = "black", size = 0.7)
```

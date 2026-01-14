# tools/write-internal-geom-data.R
# Create tiny sf objects (3 features each) for common geometry types
# and store them as *internal* package data.
#
# Run from the package root:
#   source("tools/write-internal-geom-data.R")
#
# Notes:
# - Uses EPSG:4326 (lon/lat) for simplicity.
# - Each object has 3 geometries + a small id column.

suppressPackageStartupMessages({
  library(sf)
  library(usethis)
})

crs_ll <- 2154

# 1) POINT ---------------------------------------------------------------
seq_point <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_point(c(0, 0)),
    st_point(c(1, 0)),
    st_point(c(0, 1)),
    crs = crs_ll
  )
)

# 2) MULTIPOINT ----------------------------------------------------------
seq_multipoint <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_multipoint(rbind(c(0, 0), c(0.5, 0.2), c(0.2, 0.7))),
    st_multipoint(rbind(c(1, 1), c(1.2, 1.1), c(0.9, 1.3))),
    st_multipoint(rbind(c(-1, 0), c(-0.8, 0.4), c(-1.2, 0.2))),
    crs = crs_ll
  )
)

# 3) LINESTRING ----------------------------------------------------------
seq_line <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_linestring(rbind(c(0, 0), c(1, 0), c(1, 1))),
    st_linestring(rbind(c(-1, -1), c(-0.5, -0.2), c(0, -0.5))),
    st_linestring(rbind(c(2, 0), c(2.5, 0.5), c(3, 0))),
    crs = crs_ll
  )
)

# 4) MULTILINESTRING -----------------------------------------------------
seq_multiline <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_multilinestring(list(
      rbind(c(0, 0), c(0.8, 0.2)),
      rbind(c(0.2, 0.6), c(0.9, 0.9))
    )),
    st_multilinestring(list(
      rbind(c(-1, 0), c(-1, 1), c(-0.2, 1)),
      rbind(c(-0.8, 0.2), c(-0.3, 0.6))
    )),
    st_multilinestring(list(
      rbind(c(2, 2), c(2.3, 2.6), c(2.8, 2.2)),
      rbind(c(2.1, 2.1), c(2.7, 2.7))
    )),
    crs = crs_ll
  )
)

# 5) POLYGON -------------------------------------------------------------
# Polygons must be closed rings (first point == last point)
seq_poly <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))),
    st_polygon(list(rbind(
      c(-2, -1), c(-1, -1), c(-1, 0), c(-2, 0), c(-2, -1)
    ))),
    st_polygon(list(rbind(
      c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)
    ))),
    crs = crs_ll
  )
)

# 6) MULTIPOLYGON --------------------------------------------------------
seq_multipoly <- st_sf(
  id = 1:3,
  geometry = st_sfc(
    st_multipolygon(list(
      list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))),
      list(rbind(c(1.5, 0), c(2.2, 0), c(2.2, 0.7), c(1.5, 0.7), c(1.5, 0)))
    )),
    st_multipolygon(list(
      list(rbind(c(-3, -1), c(-2, -1), c(-2, 0), c(-3, 0), c(-3, -1))),
      list(rbind(c(-2.2, 0.2), c(-1.6, 0.2), c(-1.6, 0.8), c(-2.2, 0.8), c(-2.2, 0.2)))
    )),
    st_multipolygon(list(
      list(rbind(c(3, 2), c(4, 2), c(4, 3), c(3, 3), c(3, 2))),
      list(rbind(c(4.2, 2.2), c(4.8, 2.2), c(4.8, 2.8), c(4.2, 2.8), c(4.2, 2.2)))
    )),
    crs = crs_ll
  )
)

# 7) EMPTY --------------------------------------------------------
seq_empty <- st_sf(
  geometry = st_sfc(crs = 2154)
)

# Persist as internal data ----------------------------------------------
# This writes to: R/sysdata.rda
usethis::use_data(
  seq_point,
  seq_multipoint,
  seq_line,
  seq_multiline,
  seq_poly,
  seq_multipoly,
  seq_empty,
  internal = TRUE,
  overwrite = TRUE
)

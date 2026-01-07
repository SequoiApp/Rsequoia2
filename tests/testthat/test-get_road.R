# Standard field names ----
type   <- seq_field("type")$name
nature <- seq_field("nature")$name
name   <- seq_field("name")$name
source <- seq_field("source")$name

# area_sf ----
bbox_vals <- c(xmin = 547226.9, ymin = 6794383.2,
               xmax = 549263.2, ymax = 6795983.8)
poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
area_sf <- sf::st_sf(id = 1, geometry = poly)

# empty_area_sf ----
empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

# get_road() ----
test_that("get_road() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  troncon <- get_road(area_sf)

  expect_s3_class(troncon, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(troncon)))
  expect_true(sf::st_crs(troncon)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(troncon) == "LINESTRING"))

  expect_true(
    all(troncon[[type]] %in%
          c("RN", "RD", "RC", "RF", "PN"))
  )
})

test_that("get_road() returns NULL on area with no linear infrastructure", {
  skip_on_cran()
  skip_on_ci()

  troncon_empty <- get_road(empty_area_sf)

  expect_null(troncon_empty)
})

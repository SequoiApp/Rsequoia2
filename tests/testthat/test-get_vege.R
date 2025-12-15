# Standard field names ----
type   <- seq_field("type")$name
nature <- seq_field("nature")$name
name   <- seq_field("name")$name
source <- seq_field("source")$name

# area_sf ----
bbox_vals <- c(xmin = 547226.9, ymin = 6794383.2, xmax = 549263.2, ymax = 6795983.8)
poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
area_sf <- sf::st_sf(id = 1, geometry = poly)

# empty_area_sf ----
empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

# get_vege_poly() ----
test_that("get_vege_poly() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  vege_poly <- get_vege_poly(area_sf)

  expect_s3_class(vege_poly, "sf")
  expect_true(all(c(type, source, name) %in% names(vege_poly)))
  expect_true(sf::st_crs(vege_poly)$epsg == 2154)
  expect_true(all(vege_poly[[type]] %in% c("FOR")))
  expect_true(all(sf::st_geometry_type(vege_poly) == "POLYGON"))
})

test_that("get_vege_poly() returns empty sf on area with no forest", {
  skip_on_cran()
  skip_on_ci()

  vege_poly_empty <- get_vege_poly(empty_area_sf)

  expect_s3_class(vege_poly_empty, "sf")
  expect_equal(nrow(vege_poly_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(vege_poly_empty)))
  expect_true(all(sf::st_geometry_type(vege_poly_empty) == "POLYGON"))
})

# get_vege_line() ----
test_that("get_veg_line() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  vege_line <- get_vege_line(area_sf)

  expect_s3_class(vege_line, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(vege_line)))
  expect_true(sf::st_crs(vege_line)$epsg == 2154)
  expect_true(all(vege_line[[type]] %in% c("FOR")))
  expect_true(all(sf::st_geometry_type(vege_line) == "LINESTRING"))
})

test_that("get_veg_line() returns empty sf on area with no vegetation polygons", {
  skip_on_cran()
  skip_on_ci()

  vege_line_empty <- get_vege_line(empty_area_sf)

  expect_s3_class(vege_line_empty, "sf")
  expect_equal(nrow(vege_line_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(vege_line_empty)))
  expect_true(all(sf::st_geometry_type(vege_line_empty) == "LINESTRING"))
})

# get_vege_point() ----
test_that("get_vege_point() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  vege_point <- get_vege_point(area_sf)

  expect_s3_class(vege_point, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(vege_point)))
  expect_true(sf::st_crs(vege_point)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(vege_point) == "POINT"))
  expect_true(all(vege_point[[type]] %in% c("FFF", "FFM", "FFC", "FOI", "PEU", "BOI", "VER", "VEG")))
})

test_that("get_vege_point() returns empty sf on area with no vegetation", {
  skip_on_cran()
  skip_on_ci()

  vege_point_empty <- get_vege_point(empty_area_sf)

  expect_s3_class(vege_point_empty, "sf")
  expect_equal(nrow(vege_point_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(vege_point_empty)))
  expect_true(all(sf::st_geometry_type(vege_point_empty) == "POINT"))
})

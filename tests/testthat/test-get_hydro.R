# Standard field names ----
type   <- seq_field("type")$name
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

# get_hydro_poly()  ----
test_that("get_hydro_poly() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  hydro_poly <- get_hydro_poly(area_sf)

  expect_s3_class(hydro_poly, "sf")
  expect_true(all(c(type, source, name) %in% names(hydro_poly)))
  expect_true(sf::st_crs(hydro_poly)$epsg == 2154)
  expect_true(all(hydro_poly[[type]] %in% c("SFO", "SFI")))
})

test_that("get_vege_poly() returns empty sf on area with no forest", {
  skip_on_cran()
  skip_on_ci()

  hydro_poly_empty <- get_hydro_poly(empty_area_sf)

  expect_s3_class(hydro_poly_empty, "sf")
  expect_equal(nrow(hydro_poly_empty), 0)
  expect_true(all(c(type, source, name) %in% names(hydro_poly_empty)))
  expect_true(all(sf::st_geometry_type(hydro_poly_empty) == "POLYGON"))
})

# get_vege_line() ----
test_that("get_hydro_line() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  hydro_line <- get_hydro_line(area_sf)

  expect_s3_class(hydro_line, "sf")
  expect_true(all(c(type, source, name) %in% names(hydro_line)))
  expect_true(sf::st_crs(hydro_line)$epsg == 2154)
  expect_true(all(hydro_line[[type]] %in% c("RIN", "RUI")))
  expect_true(all(sf::st_geometry_type(hydro_line) == "LINESTRING"))
})

test_that("get_hydro_line() returns empty sf on area with no vegetation polygons", {
  skip_on_cran()
  skip_on_ci()

  hydro_line_empty <- get_hydro_line(empty_area_sf)

  expect_s3_class(hydro_line_empty, "sf")
  expect_equal(nrow(hydro_line_empty), 0)
  expect_true(all(c(type, source, name) %in% names(hydro_line_empty)))
  expect_true(all(sf::st_geometry_type(hydro_line_empty) == "LINESTRING"))
})

# get_vege_point() ----
test_that("get_hydro_point() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  hydro_point <- get_hydro_point(area_sf)

  expect_s3_class(hydro_point, "sf")
  expect_true(all(c(type, source, name) %in% names(hydro_point)))
  expect_true(sf::st_crs(hydro_point)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(hydro_point) == "POINT"))
  expect_true(all(hydro_point[[type]] %in% c("MAR")))
})

test_that("get_hydro_point() returns empty sf on area with no vegetation", {
  skip_on_cran()
  skip_on_ci()

  hydro_point_empty <- get_hydro_point(empty_area_sf)

  expect_s3_class(hydro_point_empty, "sf")
  expect_equal(nrow(hydro_point_empty), 0)
  expect_true(all(c(type, source, name) %in% names(hydro_point_empty)))
  expect_true(all(sf::st_geometry_type(hydro_point_empty) == "POINT"))
})


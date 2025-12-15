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

# get_infra_poly() ----
test_that("get_infra_poly() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  infra_poly <- get_infra_poly(area_sf)

  expect_s3_class(infra_poly, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(infra_poly)))
  expect_true(sf::st_crs(infra_poly)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(infra_poly) %in% c("POLYGON", "MULTIPOLYGON")))

  expect_true(
    all(infra_poly[[type]] %in%
          c("BAT", "CIM", "CON", "CST", "AER", "SPO", "VIL", "HAB"))
  )
})

test_that("get_infra_poly() returns empty sf on area with no infrastructure", {
  skip_on_cran()
  skip_on_ci()

  infra_poly_empty <- get_infra_poly(empty_area_sf)

  expect_s3_class(infra_poly_empty, "sf")
  expect_equal(nrow(infra_poly_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(infra_poly_empty)))
  expect_true(all(sf::st_geometry_type(infra_poly_empty) == "POLYGON"))
})

# get_infra_line() ----
test_that("get_infra_line() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  infra_line <- get_infra_line(area_sf)

  expect_s3_class(infra_line, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(infra_line)))
  expect_true(sf::st_crs(infra_line)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(infra_line) == "LINESTRING"))

  expect_true(
    all(infra_line[[type]] %in%
          c("CST", "LEL", "ORO", "VFE"))
  )
})

test_that("get_infra_line() returns empty sf on area with no linear infrastructure", {
  skip_on_cran()
  skip_on_ci()

  infra_line_empty <- get_infra_line(empty_area_sf)

  expect_s3_class(infra_line_empty, "sf")
  expect_equal(nrow(infra_line_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(infra_line_empty)))
  expect_true(all(sf::st_geometry_type(infra_line_empty) == "LINESTRING"))
})

# get_infra_point() ----
test_that("get_infra_point() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  infra_point <- get_infra_point(area_sf)

  expect_s3_class(infra_point, "sf")
  expect_true(all(c(type, nature, source, name) %in% names(infra_point)))
  expect_true(sf::st_crs(infra_point)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(infra_point) == "POINT"))

  expect_true(
    all(infra_point[[type]] %in%
          c("PYL", "CLO", "CRX", "EOL", "CST", "GRO", "GOU", "ORO"))
  )
})

test_that("get_infra_point() returns empty sf on area with no point infrastructure", {
  skip_on_cran()
  skip_on_ci()

  infra_point_empty <- get_infra_point(empty_area_sf)

  expect_s3_class(infra_point_empty, "sf")
  expect_equal(nrow(infra_point_empty), 0)
  expect_true(all(c(type, nature, source, name) %in% names(infra_point_empty)))
  expect_true(all(sf::st_geometry_type(infra_point_empty) == "POINT"))
})

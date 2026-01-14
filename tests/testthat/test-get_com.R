# Standard field names ----
identifiant <- seq_field("identifiant")$name
reg_code    <- seq_field("reg_code")$name
dep_code    <- seq_field("dep_code")$name
insee       <- seq_field("insee")$name
postal      <- seq_field("postal")$name
com_name    <- seq_field("com_name")$name

# area_sf ----
bbox_vals <- c(xmin = 547226.9, ymin = 6794383.2, xmax = 549263.2, ymax = 6795983.8)
poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
area_sf <- sf::st_sf(id = 1, geometry = poly)

# empty_area_sf ----
empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

# get_hydro_poly()  ----
test_that("get_com_poly() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  com_poly <- get_com_poly(area_sf)

  expect_s3_class(com_poly, "sf")
  expect_true(all(c(identifiant, reg_code, dep_code, insee, postal, com_name) %in% names(com_poly)))
  expect_true(sf::st_crs(com_poly)$epsg == 2154)
  expect_true(nrow(com_poly) > 0)
  expect_true(all(sf::st_geometry_type(com_poly) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_true(all(sf::st_is_valid(com_poly)))
})

test_that("get_com_poly() returns NULL with no forest", {
  skip_on_cran()
  skip_on_ci()

  com_poly_empty <- get_com_poly(empty_area_sf)

  expect_null(com_poly_empty)
})

# get_com_line() ----
test_that("get_com_line() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  com_line <- get_com_line(area_sf)

  expect_s3_class(com_line, "sf")
  expect_true(sf::st_crs(com_line)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(com_line) == "MULTILINESTRING"))
})

test_that("get_com_line() returns NULL with no forest", {
  skip_on_cran()
  skip_on_ci()

  com_line_empty <- get_com_line(empty_area_sf)

  expect_null(com_line_empty)
})

test_that("get_com_line(graphic = TRUE) returns clipped lines", {
  skip_on_cran()
  skip_on_ci()

  com_line_graphic <- get_com_line(area_sf, graphic = TRUE)

  expect_s3_class(com_line_graphic, "sf")
  expect_true(nrow(com_line_graphic) > 0)
  expect_true(all(sf::st_geometry_type(com_line_graphic) == "MULTILINESTRING"))
})

test_that("graphic lines are spatially smaller than topo lines", {
  skip_on_cran()
  skip_on_ci()

  topo <- get_com_line(area_sf, graphic = FALSE)
  graphic <- get_com_line(area_sf, graphic = TRUE)

  expect_lt(
    sum(sf::st_length(graphic)),
    sum(sf::st_length(topo))
  )
})

# get_com_point() ----
test_that("get_com_point() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  com_point <- get_com_point(area_sf)

  expect_s3_class(com_point, "sf")
  expect_true(all(c(identifiant, reg_code, dep_code, insee, postal, com_name) %in% names(com_point)))
  expect_true(sf::st_crs(com_point)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(com_point) == "POINT"))
})

test_that("get_com_point() returns NULL with no forest", {
  skip_on_cran()
  skip_on_ci()

  com_point_empty <- get_com_point(empty_area_sf)

  expect_null(com_point_empty)
})

test_that("get_com_point(graphic = TRUE) returns centroids inside graphic extent", {
  skip_on_cran()
  skip_on_ci()

  com_point_graphic <- get_com_point(area_sf, graphic = TRUE)

  expect_s3_class(com_point_graphic, "sf")
  expect_true(nrow(com_point_graphic) > 0)
  expect_true(all(sf::st_geometry_type(com_point_graphic) == "POINT"))
})

# coherence ----
test_that("get_com_poly() and get_com_point() return same number of features", {
  skip_on_cran()
  skip_on_ci()

  poly <- get_com_poly(area_sf)
  point <- get_com_point(area_sf)

  expect_equal(nrow(poly), nrow(point))
})

test_that("commune centroids fall inside their polygons", {
  skip_on_cran()
  skip_on_ci()

  poly <- get_com_poly(area_sf)
  point <- get_com_point(area_sf)

  inside <- sf::st_within(point, poly, sparse = FALSE)

  expect_true(all(rowSums(inside) >= 1))
})

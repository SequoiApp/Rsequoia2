test_that("get_curves() returns null when no data", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) create_empty_sf("POINT"),
    .package = "happign"
  )

  curves <- get_curves(x)

  # tests
  expect_null(curves, "sf")
})

test_that("get_curves() works", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  line <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(1:10, , 2)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  curves <- get_curves(x)

  expect_s3_class(curves, "sf")
  expect_all_true(sf::st_geometry_type(curves) == "LINESTRING")
})

test_that("get_curves() force crs to 2154", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 4326))
  line <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(1:10, , 2)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  curves <- get_curves(x)

  expect_equal(sf::st_crs(curves)$srid, "EPSG:2154")
})

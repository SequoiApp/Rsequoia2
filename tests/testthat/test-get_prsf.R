test_that("get_prsf() returns null when no data", {
  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) create_empty_sf("POINT"),
    .package = "happign"
  )

  prsf <- get_prsf(x)

  # tests
  expect_null(prsf, "sf")
})

test_that("get_prsf() works", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  point <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) point,
    .package = "happign"
  )

  prsf <- get_prsf(x)

  expect_s3_class(prsf, "sf")
})

test_that("get_prsf() force crs to 2154", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 4326))
  point <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 4326))

  testthat::local_mocked_bindings(
    get_wfs = function(...) point,
    .package = "happign"
  )

  prsf <- get_prsf(x)

  expect_s3_class(prsf, "sf")
  expect_equal(sf::st_crs(prsf)$srid, "EPSG:2154")
})

test_that("get_forest() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  forest <- get_forest(Rsequoia2:::seq_poly, verbose = FALSE)

  # tests
  expect_null(forest, "sf")
})

test_that("get_forest() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  forest <- get_forest(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(forest, "sf")
  expect_all_true(sf::st_geometry_type(forest) == "POLYGON")
})

test_that("get_forest() force crs to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly |> sf::st_transform(4326),
    .package = "happign"
  )

  forest <- get_forest(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_equal(sf::st_crs(forest)$srid, "EPSG:2154")
})

test_that("get_forest() rejects invalid input", {
  expect_error(
    get_forest(data.frame(x = 1)),
    "must be of class"
  )
})

test_that("get_forest() rejects empty geometries", {
  expect_error(
    get_forest(sf::st_sfc(crs = 2154)),
    "must not contain empty geometries"
  )

  expect_error(
    get_forest(sf::st_sfc(sf::st_point(), crs = 2154)),
    "must not contain empty geometries"
  )
})

test_that("get_forest() handles NULL response", {
  testthat::local_mocked_bindings(
    get_wfs = function(...) NULL,
    .package = "happign"
  )

  expect_null(get_forest(Rsequoia2:::seq_poly, verbose = FALSE))
})

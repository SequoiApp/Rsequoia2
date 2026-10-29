test_that("get_forest() validates input geometry", {

  expect_error(
    get_forest(NULL, verbose = FALSE)
  )

  expect_error(
    get_forest(data.frame(x = 1), verbose = FALSE)
  )

})

test_that("get_forest() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  forest <- get_forest(
    Rsequoia2:::seq_poly,
    verbose = FALSE
  )

  expect_null(forest)

})

test_that("get_forest() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  forest <- get_forest(
    Rsequoia2:::seq_poly,
    verbose = FALSE
  )

  expect_s3_class(forest, "sf")

})

test_that("get_forest() forces crs to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  forest <- get_forest(
    Rsequoia2:::seq_poly,
    verbose = FALSE
  )

  expect_s3_class(forest, "sf")
  expect_equal(sf::st_crs(forest)$srid, "EPSG:2154")

})

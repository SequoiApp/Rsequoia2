test_that("get_old() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  old <- get_old(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_null(old)
})

test_that("get_old() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  old <- get_old(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(old, "sf")
})

test_that("get_old() force crs to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  old <- get_old(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(old, "sf")
  expect_equal(sf::st_crs(old)$srid, "EPSG:2154")
})


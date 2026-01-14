test_that("get_prsf() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  prsf <- get_prsf(Rsequoia2:::seq_poly, verbose = FALSE)

  # tests
  expect_null(prsf, "sf")
})

test_that("get_prsf() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_point,
    .package = "happign"
  )

  prsf <- get_prsf(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(prsf, "sf")
})

test_that("get_prsf() force crs to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_point,
    .package = "happign"
  )

  prsf <- get_prsf(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(prsf, "sf")
  expect_equal(sf::st_crs(prsf)$srid, "EPSG:2154")
})


test_that("get_toponyme() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  toponyme <- get_toponyme(Rsequoia2:::seq_poly, verbose = FALSE)

  # tests
  expect_null(toponyme, "sf")
})

test_that("get_toponyme() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_point,
    .package = "happign"
  )

  toponyme <- get_toponyme(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(toponyme, "sf")
})

test_that("get_toponyme() force crs to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_point,
    .package = "happign"
  )

  toponyme <- get_toponyme(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(toponyme, "sf")
  expect_equal(sf::st_crs(toponyme)$srid, "EPSG:2154")
})

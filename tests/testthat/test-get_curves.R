test_that("get_curves() returns NULL when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  curves <- get_curves(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_null(curves)
})

test_that("get_curves() works", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_line,
    .package = "happign"
  )

  curves <- get_curves(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(curves, "sf")
  expect_true(all(sf::st_geometry_type(curves) == "LINESTRING"))
})

test_that("get_curves() forces CRS to 2154", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_line |> sf::st_transform(4326),
    .package = "happign"
  )

  curves <- get_curves(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_equal(sf::st_crs(curves)$epsg, 2154)
})

test_that("get_curves() rejects invalid input", {

  expect_error(
    get_curves(data.frame(x = 1)),
    "must be"
  )
})

test_that("get_curves() handles NULL response", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) NULL,
    .package = "happign"
  )

  expect_null(get_curves(Rsequoia2:::seq_poly, verbose = FALSE))
})

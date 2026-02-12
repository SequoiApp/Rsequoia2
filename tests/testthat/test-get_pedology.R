test_that("get_pedology() validates x", {

  expect_error(get_pedology(42), "sf")
  expect_error(get_pedology("a"), "sf")

})

test_that("get_pedology() returns NULL when WFS returns no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  res <- get_pedology(Rsequoia2:::seq_poly)

  expect_null(res)

})

test_that("get_pedology() returns sf when WFS returns data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_pedology(Rsequoia2:::seq_poly)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))

})

test_that("get_pedology() returns data in EPSG:2154", {

  x <- Rsequoia2:::seq_poly |> sf::st_transform(4326)

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_pedology(x)

  expect_equal(sf::st_crs(res)$epsg, 2154)
})

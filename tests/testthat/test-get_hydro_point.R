test_that("get_hydro_point() validates x", {
  expect_error(get_hydro_point(42), "x.*sf|sfc")
  expect_error(get_hydro_point("a"), "x.*sf|sfc")
})

test_that("get_hydro_point() returns empty sf when WFS returns NULL", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(
    res <- get_hydro_point(x),
    "No hydrologic data found"
  )
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_hydro_point() returns points when data are available", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  point <- Rsequoia2:::seq_point |>
    transform(toponyme = "Paul")

  testthat::local_mocked_bindings(
    get_wfs = function(...) point,
    .package = "happign"
  )

  res <- get_hydro_point(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "POINT"))
})

test_that("get_hydro_point() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  point <- Rsequoia2:::seq_point |>
    transform(toponyme = "Paul")

  testthat::local_mocked_bindings(
    get_wfs = function(...) point,
    .package = "happign"
  )

  res <- get_hydro_point(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "MAR"))
  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

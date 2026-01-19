test_that("get_hydro_line() validates x", {
  expect_error(get_hydro_line(42), "sf")
  expect_error(get_hydro_line("a"), "sf")
})

test_that("get_hydro_line() returns empty sf when get_vege_poly() returns empty", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(res <- get_hydro_line(x), "No hydrologic data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "LINESTRING"))
})

test_that("get_hydro_line() returns lines when polygons are available", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line |>
    transform(persistance = "Permanent") |>
    transform(cpx_toponyme_de_cours_d_eau = "Paul")

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_hydro_line(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "LINESTRING"))
})

test_that("get_hydro_line() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line |>
    transform(persistance = "Permanent") |>
    transform(cpx_toponyme_de_cours_d_eau = "Paul")

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_hydro_line(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "RUP"))
  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

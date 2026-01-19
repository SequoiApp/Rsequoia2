test_that("get_hydro_poly() validates x", {
  expect_error(get_hydro_poly(42), "x.*sf")
  expect_error(get_hydro_poly("a"), "x.*sf")
})

test_that("get_hydro_poly() returns empty sf when WFS returns NULL", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(res <- get_hydro_poly(x), "No hydrologic data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_hydro_poly() returns polygon when WFS returns data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  poly <- Rsequoia2:::seq_poly |>
    transform(persistance = "Permanent") |>
    transform(cpx_toponyme_de_plan_d_eau = NA_character_)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_hydro_poly(x)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 6)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  poly <- Rsequoia2:::seq_poly |>
    transform(persistance = "Permanent") |>
    transform(cpx_toponyme_de_plan_d_eau = NA_character_)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_hydro_poly(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] %in% c("SFP", "RSO")))
  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

test_that("get_infra_line() validates x", {
  expect_error(get_infra_line(42), "x.*sf")
  expect_error(get_infra_line("a"), "x.*sf")
})

test_that("get_infra_line() returns empty sf when WFS returns no data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(
    res <- get_infra_line(x),
    "No infrastructure data found"
  )

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "LINESTRING"))
})

test_that("get_infra_line() returns lines when WFS returns data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line |>
    transform(toponyme = NA_character_, voltage = NA_real_)

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_infra_line(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "LINESTRING"))
})

test_that("get_infra_line() sets standardized fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line |>
    transform(toponyme = "Test", voltage = 225)

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_infra_line(x)

  type   <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  expect_true(type   %in% names(res))
  expect_true(source %in% names(res))

  expect_true(any(c(name, nature) %in% names(res)))

  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

test_that("get_infra_line() produces only valid type codes", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line |>
    transform(toponyme = NA_character_, voltage = 90)

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_infra_line(x)

  type <- seq_field("type")$name

  expect_true(
    all(res[[type]] %in% c("CST", "LEL", "ORO", "VFE"))
  )
})

test_that("get_infra_poly() sets type CST for constructions", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:construction_lineaire") {
        line
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_line(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "CST")
})

test_that("get_infra_line() maps electric voltage to nature field", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line
  line$voltage <- 400

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:ligne_electrique") {
        line
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_line(x)

  type   <- seq_field("type")$name
  nature <- seq_field("nature")$name

  expect_true(unique(res[[type]]) == "LEL")
  expect_all_true(res[[nature]] == 400)
})

test_that("get_infra_poly() sets type ORO for orography", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:ligne_orographique") {
        line
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_line(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "ORO")
})

test_that("get_infra_poly() sets type VFE for railways", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  line <- Rsequoia2:::seq_line

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:troncon_de_voie_ferree") {
        line
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_line(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "VFE")
})

test_that("get_infra_line() returns data in EPSG:2154", {

  x <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(2, 48)), crs = 4326)
  )

  line <- Rsequoia2:::seq_line |>
    transform(toponyme = NA_character_, voltage = NA_real_)

  testthat::local_mocked_bindings(
    get_wfs = function(...) line,
    .package = "happign"
  )

  res <- get_infra_line(x)

  expect_equal(sf::st_crs(res)$epsg, 2154)
})

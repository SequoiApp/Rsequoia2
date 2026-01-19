test_that("get_infra_poly() validates x", {
  expect_error(get_infra_poly(42), "x.*sf")
  expect_error(get_infra_poly("a"), "x.*sf")
})

test_that("get_infra_poly() returns empty sf when WFS returns no data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(
    res <- get_infra_poly(x),
    "No infrastructure data found"
  )

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
})

test_that("get_infra_poly() returns polygons when WFS returns data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  poly <- Rsequoia2:::seq_poly |>
    transform(toponyme = NA_character_, importance = 3)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_infra_poly(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
})

test_that("get_infra_poly() sets standardized fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  poly <- Rsequoia2:::seq_poly |>
    transform(toponyme = "Test", importance = 3)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type   <- seq_field("type")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  expect_true(type   %in% names(res))
  expect_true(name   %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

test_that("get_infra_poly() produces only valid type codes", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  poly <- Rsequoia2:::seq_poly |>
    transform(toponyme = NA_character_, importance = 3)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name

  expect_true(
    all(res[[type]] %in% c("BAT", "CIM", "CST", "AER", "SPO", "VIL", "HAB"))
  )
})

test_that("get_infra_poly() sets type BAT for buildings", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:batiment") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "BAT")
})

test_that("get_infra_poly() sets type CIM for cemetery", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly
  pt$toponyme <- "unlucky"

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:cimetiere") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  name <- seq_field("name")$name
  expect_true(unique(res[[type]]) == "CIM")
  expect_true(unique(res[[name]]) == "unlucky")
})

test_that("get_infra_poly() sets type CST for construction", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly
  pt$toponyme <- "tower"

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:construction_surfacique") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  name <- seq_field("name")$name
  expect_true(unique(res[[type]]) == "CST")
  expect_true(unique(res[[name]]) == "tower")
})

test_that("get_infra_poly() sets type AER for runway", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:piste_d_aerodrome") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "AER")
})

test_that("get_infra_poly() sets type SPO for sport field", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:terrain_de_sport") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  expect_true(unique(res[[type]]) == "SPO")
})

test_that("get_infra_poly() maps habitation nature to correct type", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_poly
  pt$importance = c(1, 3, 1)
  pt$toponyme  = c("A", "B", "C")

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:zone_d_habitation") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_poly(x)

  type <- seq_field("type")$name
  name <- seq_field("name")$name
  expect_equal(res[[type]], c("VIL", "HAB", "VIL"))
  expect_equal(res[[name]], c("A", "B", "C"))
})

test_that("get_infra_poly() returns data in EPSG:2154", {

  x <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(2, 48)), crs = 4326)
  )

  poly <- Rsequoia2:::seq_poly |>
    transform(toponyme = NA_character_, importance = 3)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_infra_poly(x)

  expect_equal(sf::st_crs(res)$epsg, 2154)
})

test_that("get_infra_point() validates x", {
  expect_error(get_infra_point(42), "x.*sf")
  expect_error(get_infra_point("a"), "x.*sf")
})

test_that("get_infra_point() returns empty sf when WFS returns no data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_warning(
    res <- get_infra_point(x),
    "No infrastructure data found"
  )

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_infra_point() returns points when WFS returns data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_point |>
    transform(nature = "Antenne", toponyme = NA_character_)

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  res <- get_infra_point(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_infra_point() sets standardized fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_point |>
    transform(nature = "Antenne", toponyme = "Test")

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  res <- get_infra_point(x)

  type   <- seq_field("type")$name
  name   <- seq_field("name")$name
  source <- seq_field("source")$name

  expect_true(type   %in% names(res))
  expect_true(name   %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[source]] == "IGNF_BDTOPO_V3"))
})

test_that("get_infra_point() produces only valid type codes", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- rbind(Rsequoia2:::seq_point,
              Rsequoia2:::seq_point)
  pt$nature <- c(
    "Antenne", "Clocher", "Croix",
    "Calvaire", "Eolienne", "Autre"
  )
  pt$toponyme <- NA_character_

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  res <- get_infra_point(x)

  type <- seq_field("type")$name

  expect_true(
    all(res[[type]] %in% c("PYL", "CLO", "CRX", "EOL", "CST", "GRO", "GOU", "ORO"))
  )
})

test_that("get_infra_point() maps construction nature to correct type", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- rbind(Rsequoia2:::seq_point,
              Rsequoia2:::seq_point)
  pt$nature <- c(
    "Antenne", "Clocher", "Croix",
    "Calvaire", "Eolienne", "Autre"
  )
  pt$toponyme <- NA_character_

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:construction_ponctuelle") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_point(x)

  type <- seq_field("type")$name

  expect_true(any(res[[type]] == "PYL"))
  expect_true(any(res[[type]] == "CLO"))
  expect_true(any(res[[type]] == "CRX"))
  expect_true(any(res[[type]] == "EOL"))
  expect_true(any(res[[type]] == "CST"))
})

test_that("get_infra_point() maps orography nature to correct type", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_point
  pt$nature <- c("Grotte", "Gouffre", "Sommet")
  pt$toponyme <- "Topo"

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:detail_orographique") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_point(x)

  type <- seq_field("type")$name

  expect_true(any(res[[type]] == "GRO"))
  expect_true(any(res[[type]] == "GOU"))
  expect_true(any(res[[type]] == "ORO"))
})

test_that("get_infra_point() sets type PYL for pylons", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  pt <- Rsequoia2:::seq_point
  pt$nature   = "Antenne"
  pt$toponyme = NA_character_

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:construction_ponctuelle") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_point(x)

  type <- seq_field("type")$name

  expect_true(unique(res[[type]]) == "PYL")
})

test_that("get_infra_point() drops Z and M dimensions", {

  x <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(5, 5, 10)), crs = 2154)
  )

  pt <- sf::st_sf(
    nature   = "Antenne",
    toponyme = NA_character_,
    geometry = sf::st_sfc(sf::st_point(c(5, 5, 3)), crs = 2154)
  )

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      if (layer == "BDTOPO_V3:construction_ponctuelle") {
        pt
      } else {
        Rsequoia2:::seq_empty
      }
    },
    .package = "happign"
  )

  res <- get_infra_point(x)

  geoms <- sf::st_geometry(res)
  has_z <- vapply(geoms, function(g) length(sf::st_coordinates(g)[1, ]) > 2, logical(1))
  expect_false(any(has_z))

})

test_that("get_infra_point() returns data in EPSG:2154", {

  x <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(2, 48)), crs = 4326)
  )

  pt <- Rsequoia2:::seq_point
  pt$nature <- "Antenne"
  pt$toponyme <- "NA_character_"

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  res <- get_infra_point(x)

  expect_equal(sf::st_crs(res)$epsg, 2154)
})

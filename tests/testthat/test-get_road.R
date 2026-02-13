test_that("get_road() returns NULL when WFS returns no features", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  road <- get_road(Rsequoia2:::seq_poly)

  # tests
  expect_null(road, "sf")
})

test_that("get_road() classifies revetue roads correctly", {

  tr_mock <- sf::st_sf(
    nature = c("Route a 1 chaussee", "Route a 1 chaussee"),
    importance = c(3, 3),
    cpx_numero = c("N12", "D45"),
    cpx_toponyme_route_nommee = c(NA, "Route de Test"),
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) tr_mock,
    .package = "happign"
  )

  road <- get_road(Rsequoia2:::seq_poly)

  expect_s3_class(road, "sf")
  expect_equal(road[[seq_field("type")$name]], c("RN", "RD"))
  expect_equal(
    road[[seq_field("name")$name]],
    c("N12", "D45")
  )
  expect_equal(
    unique(road[[seq_field("source")$name]]),
    "BDTOPO V3"
  )
})

test_that("get_road() classifies natural paths as PN", {

  tr_mock <- sf::st_sf(
    nature = c("Chemin", "Sentier"),
    importance = c(NA, NA),
    cpx_numero = c("", ""),
    cpx_toponyme_route_nommee = c("Chemin du bois", "Sentier rural"),
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) tr_mock,
    .package = "happign"
  )

  road <- get_road(Rsequoia2:::seq_poly)

  expect_equal(
    unique(road[[seq_field("type")$name]]),
    "PN"
  )
})

test_that("get_road() classifies bretelles as RN when importance >= 2", {

  tr_mock <- sf::st_sf(
    nature = "Bretelle",
    importance = 2,
    cpx_numero = NA,
    cpx_toponyme_route_nommee = "Bretelle A10",
    geometry = sf::st_sfc(
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      sf::st_linestring(
        matrix(c(0, 0,
                 1, 1),
               ncol = 2,
               byrow = TRUE)
      ),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) tr_mock,
    .package = "happign"
  )

  road <- get_road(Rsequoia2:::seq_poly)

  expect_equal(road[[seq_field("type")$name]], c("RN", "RN"))
})






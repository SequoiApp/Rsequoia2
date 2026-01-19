test_that("get_vege_poly() validates x", {
  expect_error(get_vege_poly(42), "x.*sf")
  expect_error(get_vege_poly("a"), "x.*sf")
})

test_that("get_vege_poly() returns empty sf when WFS returns NULL", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) NULL,
    .package = "happign"
  )

  expect_warning(res <- get_vege_poly(x), "No vegetation data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() returns polygon when WFS returns data", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 2154
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_vege_poly(x)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() remove interior ring", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  outer <- rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
  hole <- rbind(c(0.25, 0.25), c(0.75, 0.25), c(0.75, 0.75), c(0.25, 0.75), c(0.25, 0.25))
  poly <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = 2154)

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_vege_poly(x)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 2154
  )

  testthat::local_mocked_bindings(
    get_wfs = function(...) poly,
    .package = "happign"
  )

  res <- get_vege_poly(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "FOR"))
  expect_true(all(res[[source]] == "ignf_masque_foret"))
})

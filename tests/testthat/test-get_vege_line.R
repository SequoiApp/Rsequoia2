test_that("get_vege_line() validates x", {
  expect_error(get_vege_line(42), "sf")
  expect_error(get_vege_line("a"), "sf")
})

test_that("get_vege_line() returns empty sf when get_vege_poly() returns empty", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  empty_poly <- create_empty_sf("POLYGON") |> seq_normalize("vct_poly")

  testthat::local_mocked_bindings(
    get_vege_poly = function(...) empty_poly
  )

  expect_warning(res <- get_vege_line(x), "No vegetation data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "LINESTRING"))
})

test_that("get_vege_line() returns lines when polygons are available", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  poly <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))
      )),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    get_vege_poly = function(...) poly
  )

  res <- get_vege_line(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "LINESTRING"))
})

test_that("get_vege_line() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  poly <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))
      )),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    get_vege_poly = function(...) poly
  )

  res <- get_vege_line(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "FOR"))
  expect_true(all(res[[source]] == "ignf_masque_foret"))
})

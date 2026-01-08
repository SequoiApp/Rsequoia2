test_that("get_vege_point() validates x", {
  expect_error(get_vege_point(42), "x.*sf|sfc")
  expect_error(get_vege_point("a"), "x.*sf|sfc")
})

test_that("get_vege_point() returns empty sf when WFS returns NULL", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  testthat::local_mocked_bindings(
    get_wfs = function(...) NULL,
    .package = "happign"
  )

  expect_warning(res <- get_vege_point(x), "No vegetation data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_vege_point() returns empty sf when no valid code_tfv", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  fv <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0))
      )),
      crs = 2154
    )
  )
  fv$code_tfv <- "XXX"

  testthat::local_mocked_bindings(
    get_wfs = function(...) fv,
    .package = "happign"
  )

  expect_warning(res <- get_vege_point(x), "No vegetation data found")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_vege_point() returns points when vegetation is available", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  fv <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(10000,0), c(10000,10000), c(10000), c(0,0))
      )),
      crs = 2154
    )
  )
  fv$code_tfv <- "FF1"

  testthat::local_mocked_bindings(
    get_wfs = function(...) fv,
    .package = "happign"
  )

  res <- get_vege_point(x)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_vege_point() sets type and source fields", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  fv <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(10000,0), c(10000, 10000), c(0,10000), c(0,0))
      )),
      crs = 2154
    )
  )
  fv$code_tfv <- "FO2"  # Resineux

  testthat::local_mocked_bindings(
    get_wfs = function(...) fv,
    .package = "happign"
  )

  res <- get_vege_point(x)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "Resineux"))
  expect_true(all(res[[source]] == "ignf_bd_foretv2"))
})

test_that("get_vege_point() returns empty when area is too small", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154))

  fv <- sf::st_as_sf(
    sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))  # 1 m²
      )),
      crs = 2154
    )
  )
  fv$code_tfv <- "FF1"

  testthat::local_mocked_bindings(
    get_wfs = function(...) fv,
    .package = "happign"
  )


  expect_warning(res <- get_vege_point(x), "No vegetation data found")

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POINT"))
})

test_that("get_ars() validates x", {
  expect_error(get_ars(42), "x.*sf")
  expect_error(get_ars("a"), "x.*sf")
})

test_that("get_ars() validates type", {
  x <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_ars(x, key = "bad_key"), "Available keys")
  expect_error(get_ars(x, key = c("key1", "key2")), "must contain exactly one element")
})

test_that("get_ars() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(7.4158, 48.8627)), crs = 4326))

  ars <- quiet(get_ars(x, key = "captage", buffer = 10))

  expect_s3_class(ars, "sf")
  expect_true(nrow(ars) >= 1)
  expect_equal(sf::st_crs(ars), sf::st_crs(2154))
  expect_true("type" %in% names(ars))

})

test_that("get_ars() works with mocked WFS", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(7.4158, 48.8627)), crs = 4326))

  local_mocked_bindings(
    get_wfs = function(x, layer, overwrites, spatial_filter) x,
    .package = "happign"
  )

  ars <- get_ars(x, key = "captage", buffer = 10)

  expect_s3_class(ars, "sf")
  expect_true(nrow(ars) >= 1)
  expect_equal(sf::st_crs(ars), sf::st_crs(2154))
  expect_true("type" %in% names(ars))

})

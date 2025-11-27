test_that("get_mnhn() validates x", {
  expect_error(get_mnhn(42), "x.*sf")
  expect_error(get_mnhn("a"), "x.*sf")
})

test_that("get_mnhn() validates type", {
  x <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_mnhn(x, key = "bad_key"), "available layers")
  expect_error(get_mnhn(x, key = c("key1", "key2")), "must contain exactly one element")
})

test_that("get_mnhn() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  x <- sf::st_sfc(sf::st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)

  ospar <- quiet(get_mnhn(x, key = "ospar", buffer = 500))

  expect_s3_class(ospar, "sf")
  expect_true(nrow(ospar) >= 1)
  expect_equal(sf::st_crs(ospar), sf::st_crs(2154))

})

test_that("get_mnhn() works with mocked WFS", {

  x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))

  local_mocked_bindings(
    get_wfs = function(x, layer, overwrites, spatial_filter) x,
    .package = "happign"
  )

  ospar <- get_mnhn(x, key = "ospar", buffer = 1)

  expect_s3_class(ospar, "sf")
  expect_equal(sf::st_crs(ospar), sf::st_crs(2154))

})


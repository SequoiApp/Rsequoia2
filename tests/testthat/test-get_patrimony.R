test_that("get_patrimony() validates x", {
  expect_error(get_patrimony(42), "x.*sf")
  expect_error(get_patrimony("a"), "x.*sf")
})

test_that("get_patrimony() validates type", {
  x <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_patrimony(x, key = "bad_key"), "available layers")
  expect_error(get_patrimony(x, key = c("key1", "key2")), "must contain exactly one element")
})

test_that("get_patrimony() works (local only)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  x <- sf::st_sf(sf::st_sfc(sf::st_point(c(7.410821, 48.854160)), crs = 4326))

  immh <- get_patrimony(x, key = "immh", buffer = 500)

  expect_s3_class(immh, "sf")
  expect_true(nrow(immh) >= 1)
  expect_equal(sf::st_crs(immh), sf::st_crs(2154))

})

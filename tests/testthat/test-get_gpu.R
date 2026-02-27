test_that("get_gpu() validates x", {
  expect_error(get_gpu(42), "x.*sf")
  expect_error(get_gpu("a"), "x.*sf")
})

test_that("get_gpu() validates type", {
  x <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_gpu(x, key = "bad_key"), "available layers")
  expect_error(get_gpu(x, key = c("key1", "key2")), "must contain exactly one element")
})

test_that("get_gpu() works (local only)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  x <- sf::st_sf(sf::st_sfc(sf::st_point(c(7.410821, 48.854160)), crs = 4326))

  gpu <- get_gpu(x, key = "v.gpu.municipality.poly", buffer = 500)

  expect_s3_class(gpu, "sf")
  expect_true(nrow(gpu) >= 1)
})

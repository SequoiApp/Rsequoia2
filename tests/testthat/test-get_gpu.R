test_that("get_gpu() validates x", {
  expect_error(get_gpu(42, "municipality"), "x.*sf")
  expect_error(get_gpu("a", "municipality"), "x.*sf")
})

test_that("get_gpu() validates layer", {
  x <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

  expect_error(get_gpu(x, "bad_layer"), "Invalid.*layer")
  expect_error(get_gpu(x, c("municipality", "document")), "Invalid.*layer")
})

test_that("get_gpu() returns downloaded layer", {
  x <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

  local_mocked_bindings(
    get_apicarto_gpu = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  gpu <- get_gpu(x, "municipality", verbose = FALSE)

  expect_s3_class(gpu, "sf")
  expect_gt(nrow(gpu), 0)
})

test_that("get_scan() validates x", {
  expect_error(get_scan(42), "x.*sf")
  expect_error(get_scan("a"), "x.*sf")
})

test_that("get_scan() validates type", {
  p <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_scan(p, type = "foo"), "type")
  expect_error(get_scan(p, type = c("scan25","scan100")), "type")
})

test_that("get_scan() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  p <- sf::st_sfc(sf::st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)

  scan <- quiet(get_scan(p, type = "scan25", buffer = 10))

  expect_s4_class(scan, "SpatRaster")
  expect_true(terra::ncell(scan) > 0)
  expect_match(terra::crs(scan), "2154")

})

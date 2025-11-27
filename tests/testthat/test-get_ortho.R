test_that("get_ortho() validates x", {
  expect_error(get_ortho(42), "x.*sf")
  expect_error(get_ortho("a"), "x.*sf")
})

test_that("get_ortho() validates type", {
  p <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_ortho(p, type = "foo"), "type")
  expect_error(get_ortho(p, type = c("irc","rgb")), "type")
})

test_that("get_ortho() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  p <- sf::st_sfc(sf::st_point(c(-4.372746579180652, 47.79820761331345)), crs = 4326)

  ortho <- quiet(get_ortho(p, type = "rgb", buffer = 10, zoom = 7, crs = 2154))

  expect_s4_class(ortho, "SpatRaster")
  expect_true(terra::ncell(ortho) > 0)
  expect_match(terra::crs(ortho), "2154")

})

test_that("get_ortho() works with mocked WMTS", {

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "EPSG:2154"
  )
  terra::values(r_fake) <- 1

  # Minimal sf input
  p <- sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154)

  # mock get_wmts
  local_mocked_bindings(
    get_wmts = function(x, layer, crs, zoom, verbose, overwrite) r_fake,
    .package = "happign"
  )

  ortho <- get_ortho(p, type = "rgb", buffer = 1)

  # tests
  expect_s4_class(ortho, "SpatRaster")
  expect_equal(terra::crs(ortho), terra::crs("epsg:2154"))

  # check that masking produced a raster with NA outside polygon
  expect_all_true(c(NA, 1) %in% terra::values(ortho))
})

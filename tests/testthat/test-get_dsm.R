test_that("get_dsm() errors with non spatial input", {
  expect_error(get_dsm(42), "must be.*sf.*sfc")
  expect_error(get_dsm("abc"), "must be.*sf.*sfc")
})

test_that("get_dsm() works ", {

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )
  terra::values(r_fake) <- 1

  # Minimal sf input
  x <- sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154)

  # mock get_wmts
  local_mocked_bindings(
    get_wms_raster = function(x, layer, res, rgb, crs, verbose) r_fake,
    .package = "happign"
  )

  dsm <- get_dsm(x, buffer = 1, verbose = FALSE)

  # tests
  expect_s4_class(dsm, "SpatRaster")
  expect_shape(dsm, dim = c(10, 10, 1))

})

test_that("get_dsm() masks outside buffer area", {

  r_fake <- terra::rast(nrows = 10, ncols = 10,
                        xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "epsg:2154")
  terra::values(r_fake) <- 1

  x <- sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154)

  local_mocked_bindings(
    get_wms_raster = function(...) r_fake,
    .package="happign"
  )

  dsm <- get_dsm(x, buffer = 0.1, verbose=FALSE)

  vals <- terra::values(dsm)
  expect_true(any(is.na(vals)))
  expect_true(any(vals == 1))
})

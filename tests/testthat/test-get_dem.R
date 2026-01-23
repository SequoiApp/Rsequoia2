test_that("get_dem() errors with non spatial input", {
  expect_error(get_dem(42), "must be.*sf.*sfc")
  expect_error(get_dem("abc"), "must be.*sf.*sfc")
})

test_that("get_dem() works", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )
  terra::values(r_fake) <- 1

  local_mocked_bindings(
    get_wms_raster = function(..., filename){
      writeRaster(r_fake, filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  dem <- get_dem(Rsequoia2:::seq_point, buffer = 1, verbose = FALSE)

  # tests
  expect_s4_class(dem, "SpatRaster")
  expect_shape(dem, dim = c(10, 10, 1))

})

test_that("get_dem() masks outside buffer area", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )
  terra::values(r_fake) <- 1

  local_mocked_bindings(
    get_wms_raster = function(..., filename){
      writeRaster(r_fake, filename = filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  dem <- get_dem(Rsequoia2:::seq_point, buffer = 0.1, verbose = FALSE)

  vals <- terra::values(dem)
  expect_true(any(is.na(vals)))
  expect_true(any(vals == 1))
})

test_that("get_dem() calls get_wms_raster once per envelope row", {

  skip_on_os("mac")

  calls <- 0

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )
  terra::values(r_fake) <- 1

  local_mocked_bindings(
    get_wms_raster = function(..., filename){
      calls <<- calls + 1
      writeRaster(r_fake, filename = filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  x <- Rsequoia2:::seq_poly
  b <- 0.1
  get_dem(Rsequoia2:::seq_poly, buffer = b, verbose = FALSE)

  expect_equal(calls, nrow(envelope(x, b)))
})

test_that("get_dem() writes temporary raster files", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )
  terra::values(r_fake) <- 1

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  dem <- get_dem(Rsequoia2:::seq_point, buffer = 1, verbose = FALSE)

  files <- list.files(tempdir(), pattern = "^r_\\d{3}\\.tif$", full.names = TRUE)

  expect_true(length(files) >= 1)
})

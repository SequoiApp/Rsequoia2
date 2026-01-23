test_that("get_ortho() validates x", {
  expect_error(get_ortho(42), "x.*sf")
  expect_error(get_ortho("a"), "x.*sf")
})

test_that("get_ortho() validates type", {
  p <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_ortho(p, type = "foo"), "type")
  expect_error(get_ortho(p, type = c("irc","rgb")), "type")
})

test_that("get_ortho() works", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10, nlyrs = 4,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )

  # Fill with valid 8-bit RGB values
  terra::values(r_fake) <- cbind(
    rep(255, terra::ncell(r_fake)),  # red
    rep(128, terra::ncell(r_fake)),  # green
    rep(64,  terra::ncell(r_fake)),   # blue
    rep(0,  terra::ncell(r_fake))   # blue
  )

  local_mocked_bindings(
    get_wmts = function(..., filename){
      writeRaster(r_fake, filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  ortho <- get_ortho(Rsequoia2:::seq_point, type = "irc", buffer = 1, verbose = FALSE)

  # tests
  expect_s4_class(ortho, "SpatRaster")
  expect_shape(ortho, dim = c(10, 10, 4))
  expect_true(terra::has.RGB(ortho))
  expect_equal(names(ortho), c("red", "green", "blue", "alpha"))

})

test_that("get_ortho() masks outside buffer area", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10, nlyrs = 4,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )

  # Fill with valid 8-bit RGB values
  terra::values(r_fake) <- cbind(
    rep(255, terra::ncell(r_fake)),  # red
    rep(128, terra::ncell(r_fake)),  # green
    rep(64,  terra::ncell(r_fake)),   # blue
    rep(0,  terra::ncell(r_fake))   # blue
  )

  local_mocked_bindings(
    get_wmts = function(..., filename){
      writeRaster(r_fake, filename = filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  ortho <- get_ortho(Rsequoia2:::seq_point, type = "irc", buffer = 0.1, verbose = FALSE)

  vals <- terra::values(ortho)
  expect_true(any(is.na(vals)))
  expect_true(any(vals == 128))
})

test_that("get_ortho() calls get_wmts once per envelope row", {

  skip_on_os("mac")

  calls <- 0

  r_fake <- terra::rast(
    nrows = 10, ncols = 10, nlyrs = 4,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )

  # Fill with valid 8-bit RGB values
  terra::values(r_fake) <- cbind(
    rep(255, terra::ncell(r_fake)),  # red
    rep(128, terra::ncell(r_fake)),  # green
    rep(64,  terra::ncell(r_fake)),   # blue
    rep(0,  terra::ncell(r_fake))   # blue
  )

  local_mocked_bindings(
    get_wmts = function(..., filename){
      calls <<- calls + 1
      writeRaster(r_fake, filename = filename, overwrite = TRUE)}
    ,
    .package = "happign"
  )

  x <- Rsequoia2:::seq_poly
  b <- 0.1
  get_ortho(Rsequoia2:::seq_poly, type = "irc", buffer = b, verbose = FALSE)

  expect_equal(calls, nrow(envelope(x, b)))
})

test_that("get_ortho() writes temporary raster files", {

  skip_on_os("mac")

  r_fake <- terra::rast(
    nrows = 10, ncols = 10, nlyrs = 4,
    xmin = 0, xmax = 10,
    ymin = 0, ymax = 10,
    crs = "epsg:2154"
  )

  # Fill with valid 8-bit RGB values
  terra::values(r_fake) <- cbind(
    rep(255, terra::ncell(r_fake)),  # red
    rep(128, terra::ncell(r_fake)),  # green
    rep(64,  terra::ncell(r_fake)),   # blue
    rep(0,  terra::ncell(r_fake))   # blue
  )

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  ortho <- get_ortho(Rsequoia2:::seq_point, type = "irc", buffer = 1, verbose = FALSE)

  files <- list.files(tempdir(), pattern = "^r_\\d{3}\\.tif$", full.names = TRUE)

  expect_true(length(files) >= 1)
})

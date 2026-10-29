test_that("get_climate() reads and filters climate data", {

  raster_path <- tempfile(fileext = ".tif")
  on.exit(unlink(raster_path), add = TRUE)

  climate_raster <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 30,
    ymin = 0,
    ymax = 30,
    crs = "EPSG:2154"
  )

  terra::values(climate_raster) <- 1:9

  terra::writeRaster(
    climate_raster,
    raster_path,
    overwrite = TRUE
  )

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0),
        c(20, 0),
        c(20, 20),
        c(0, 20),
        c(0, 0)
      ))),
      crs = 2154
    )
  )

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    download_climate = function(
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
    ) {
      tracker$called <- TRUE
      raster_path
    }
  )

  out <- get_climate(
    x = x,
    buffer = 0,
    verbose = FALSE
  )

  expect_true(tracker$called)
  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::nlyr(out), 1L)
  expect_true(any(!is.na(terra::values(out))))
  expect_true(
    terra::xmax(terra::ext(out)) <= 20
  )
  expect_true(
    terra::ymax(terra::ext(out)) <= 20
  )

  climate_levels <- terra::levels(out)[[1]]

  expect_named(climate_levels, c("value", "label"))
  expect_equal(nrow(climate_levels), 9L)
  expect_equal(
    climate_levels$label[1],
    "climat de montagne"
  )
  expect_equal(
    climate_levels$label[9],
    "non class\u00E9"
  )
})

test_that("get_climate() forwards download arguments", {

  raster_path <- tempfile(fileext = ".tif")
  on.exit(unlink(raster_path), add = TRUE)

  climate_raster <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 30,
    ymin = 0,
    ymax = 30,
    crs = "EPSG:2154"
  )

  terra::values(climate_raster) <- 1:9

  terra::writeRaster(
    climate_raster,
    raster_path,
    overwrite = TRUE
  )

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(20, 0),
      c(20, 20),
      c(0, 20),
      c(0, 0)
    ))),
    crs = 2154
  )

  tracker <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    download_climate = function(
    cache = NULL,
    overwrite = FALSE,
    verbose = TRUE
    ) {
      tracker$cache <- cache
      tracker$overwrite <- overwrite
      tracker$verbose <- verbose
      raster_path
    }
  )

  get_climate(
    x = x,
    buffer = 0,
    cache = "custom_cache",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(tracker$cache, "custom_cache")
  expect_true(tracker$overwrite)
  expect_false(tracker$verbose)
})

test_that("get_climate() validates input geometry", {

  expect_error(
    get_climate(
      x = data.frame(id = 1),
      verbose = FALSE
    ),
    "must be of class"
  )

  x_empty <- sf::st_sfc(
    sf::st_point(),
    crs = 2154
  )

  expect_error(
    get_climate(
      x = x_empty,
      verbose = FALSE
    ),
    "must not contain empty geometries"
  )
})

test_that("get_climate() requires positive buffer for points and lines", {

  x <- sf::st_sfc(
    sf::st_point(c(10, 10)),
    crs = 2154
  )

  expect_error(
    get_climate(
      x = x,
      buffer = 0,
      verbose = FALSE
    ),
    "buffer.*positive"
  )
})

test_that("get_climate() errors when raster has several layers", {

  raster_path <- tempfile(fileext = ".tif")
  on.exit(unlink(raster_path), add = TRUE)

  r <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 30,
    ymin = 0,
    ymax = 30,
    crs = "EPSG:2154"
  )

  terra::values(r) <- 1:9

  terra::writeRaster(
    c(r, r),
    raster_path,
    overwrite = TRUE
  )

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(20, 0),
      c(20, 20),
      c(0, 20),
      c(0, 0)
    ))),
    crs = 2154
  )

  testthat::local_mocked_bindings(
    download_climate = function(...) raster_path
  )

  expect_error(
    get_climate(
      x = x,
      buffer = 0,
      verbose = FALSE
    ),
    "exactly one layer"
  )
})

test_that("get_climate() errors when raster does not overlap x", {

  raster_path <- tempfile(fileext = ".tif")
  on.exit(unlink(raster_path), add = TRUE)

  r <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 1000,
    xmax = 1030,
    ymin = 1000,
    ymax = 1030,
    crs = "EPSG:2154"
  )

  terra::values(r) <- 1:9

  terra::writeRaster(
    r,
    raster_path,
    overwrite = TRUE
  )

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(20, 0),
      c(20, 20),
      c(0, 20),
      c(0, 0)
    ))),
    crs = 2154
  )

  testthat::local_mocked_bindings(
    download_climate = function(...) raster_path
  )

  expect_error(
    get_climate(
      x = x,
      buffer = 0,
      verbose = FALSE
    ),
    "does not overlap"
  )
})

test_that("get_climate() propagates download errors", {

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0),
      c(20, 0),
      c(20, 20),
      c(0, 20),
      c(0, 0)
    ))),
    crs = 2154
  )

  testthat::local_mocked_bindings(
    download_climate = function(...) {
      cli::cli_abort("Download failed.")
    }
  )

  expect_error(
    get_climate(
      x = x,
      verbose = FALSE
    ),
    "Download failed"
  )
})

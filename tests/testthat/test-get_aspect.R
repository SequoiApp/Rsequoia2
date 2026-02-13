test_that("get_aspect() errors when mixing x with dem", {

  pt <- sf::st_sfc(sf::st_point(c(1,1)), crs=2154)
  fake_dem <- terra::rast()

  expect_error(
    get_aspect(x = pt, dem = fake_dem),
    "cannot be used together"
  )
})

test_that("get_aspect() errors when no DEM is provided in manual mode", {

  expect_error(get_aspect(x = NULL, dem = NULL), " must be provided")

})

test_that("get_aspect() calls get_dem() when x is provided", {

  skip_on_os("mac")

  pt <- sf::st_sfc(sf::st_point(c(1,1)), crs=2154)

  fake_dem <- terra::rast(nrows=2, ncols=2,
                          xmin=0, xmax=2, ymin=0, ymax=2, crs="epsg:2154")
  terra::values(fake_dem) <- 1

  tracker <- new.env(parent = emptyenv())
  tracker$dem_called <- FALSE

  local_mocked_bindings(
    get_dem = function(x, res, ...) { tracker$dem_called <- TRUE ; fake_dem}
  )

  s <- get_aspect(x = pt, agg = 5, verbose = FALSE)

  expect_true(tracker$dem_called)
  expect_s4_class(s, "SpatRaster")
})

test_that("get_aspect() computes aspect from DEM in manual mode", {

  skip_on_os("mac")

  # DEM with simple gradient: aspect should not be all zeros
  dem <- terra::rast(nrows=3, ncols=3, xmin=0, xmax=3, ymin=0, ymax=3, crs="epsg:2154")

  terra::values(dem) <- matrix(c(
    10, 40, 80,
    10, 40, 80,
    10, 40, 80
  ), nrow=3, byrow=TRUE)

  aspect <- get_aspect(dem = dem, agg = 1, verbose = FALSE)

  expect_s4_class(aspect, "SpatRaster")
  expect_true(any(terra::values(aspect) != 0, na.rm=TRUE))
})

test_that("get_aspect() aggregates DEM when resolution < agg", {

  skip_on_os("mac")

  dem <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10, crs="epsg:2154")
  terra::values(dem) <- 1

  aspect <- get_aspect(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(aspect), c(5, 5))
})

test_that("get_aspect() does NOT aggregate when resolution >= agg", {

  skip_on_os("mac")

  dem <- terra::rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, crs="epsg:2154")
  # resolution = 10m >= agg = 5 -> no aggregation

  aspect <- get_aspect(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(aspect), c(10, 10))
})

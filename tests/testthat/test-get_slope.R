test_that("get_slope() errors when mixing x with dem", {

  pt <- sf::st_sfc(sf::st_point(c(1,1)), crs=2154)
  fake_dem <- terra::rast()

  expect_error(
    get_slope(x = pt, dem = fake_dem),
    "cannot be used together"
  )
})

test_that("get_slope() errors when no DEM is provided in manual mode", {

  expect_error(get_slope(x = NULL, dem = NULL), " must be provided")

})

test_that("get_slope() calls get_dem() when x is provided", {

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

  s <- get_slope(x = pt, agg = 5, verbose = FALSE)

  expect_true(tracker$dem_called)
  expect_s4_class(s, "SpatRaster")
})

test_that("get_slope() computes slope from DEM in manual mode", {

  skip_on_os("mac")

  # DEM with simple gradient: slope should not be all zeros
  dem <- terra::rast(nrows=3, ncols=3, xmin=0, xmax=3, ymin=0, ymax=3, crs="epsg:2154")

  terra::values(dem) <- matrix(c(
    10, 40, 80,
    10, 40, 80,
    10, 40, 80
  ), nrow=3, byrow=TRUE)

  slope <- get_slope(dem = dem, agg = 1, verbose = FALSE)

  expect_s4_class(slope, "SpatRaster")
  expect_true(any(terra::values(slope) != 0, na.rm=TRUE))
})

test_that("get_slope() aggregates DEM when resolution < agg", {

  skip_on_os("mac")

  dem <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10, crs="epsg:2154")
  terra::values(dem) <- 1

  slope <- get_slope(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(slope), c(5, 5))
})

test_that("get_slope() does NOT aggregate when resolution >= agg", {

  skip_on_os("mac")

  dem <- terra::rast(nrows=2, ncols=2, xmin=0, xmax=20, ymin=0, ymax=20, crs="epsg:2154")
  # resolution = 10m >= agg = 5 → no aggregation

  slope <- get_slope(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(slope), c(10, 10))
})

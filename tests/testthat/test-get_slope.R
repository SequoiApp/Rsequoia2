make_dem <- function(values = NULL, n = 3, res = 1, crs = "epsg:2154") {
  dem <- terra::rast(
    nrows = n,
    ncols = n,
    xmin = 0,
    xmax = n * res,
    ymin = 0,
    ymax = n * res,
    crs = crs
  )

  if (is.null(values)) {
    values <- seq_len(n * n)
  }

  terra::values(dem) <- values
  dem
}

test_that("get_slope() rejects ambiguous input", {

  pt <- sf::st_sfc(sf::st_point(c(1, 1)), crs = 2154)
  dem <- make_dem()

  expect_error(
    get_slope(x = pt, dem = dem),
    "cannot be used together"
  )
})

test_that("get_slope() errors when no DEM is provided in manual mode", {

  expect_error(get_slope(x = NULL, dem = NULL), " must be provided")

})

test_that("get_slope() calls get_dem() when x is provided", {
  skip_on_os("mac")

  pt <- sf::st_sfc(sf::st_point(c(1, 1)), crs = 2154)
  dem <- make_dem()

  dem_called <- FALSE
  local_mocked_bindings(
    get_dem = function(x, res, ...) {
      dem_called <<- TRUE
      expect_equal(res, 5)
      dem
    }
  )

  slope <- get_slope(x = pt, agg = 5, verbose = FALSE)

  expect_true(dem_called)
  expect_s4_class(slope, "SpatRaster")
  expect_equal(names(slope), "slope")
})

test_that("get_slope() computes slope from DEM", {
  skip_on_os("mac")

  dem <- make_dem(
    values = matrix(c(
      10, 40, 80,
      10, 40, 80,
      10, 40, 80
    ), nrow = 3, byrow = TRUE)
  )

  slope <- get_slope(dem = dem, agg = 1, verbose = FALSE)

  expect_s4_class(slope, "SpatRaster")
  expect_equal(names(slope), "slope")
  expect_true(any(terra::values(slope) != 0, na.rm = TRUE))
})

test_that("get_slope() defaults to percent", {
  skip_on_os("mac")

  dem <- make_dem(
    values = matrix(c(
      10, 40, 80,
      10, 40, 80,
      10, 40, 80
    ), nrow = 3, byrow = TRUE)
  )

  default <- get_slope(dem = dem, agg = 1, verbose = FALSE)
  percent <- get_slope(dem = dem, agg = 1, unit = "percent", verbose = FALSE)

  expect_equal(
    as.vector(terra::values(default)),
    as.vector(terra::values(percent)),
    tolerance = 1e-6
  )
})

test_that("get_slope() supports radians", {
  skip_on_os("mac")

  dem <- make_dem(
    values = matrix(c(
      10, 40, 80,
      10, 40, 80,
      10, 40, 80
    ), nrow = 3, byrow = TRUE)
  )

  degrees <- get_slope(dem = dem, agg = 1, unit = "degrees", verbose = FALSE)
  radians <- get_slope(dem = dem, agg = 1, unit = "radians", verbose = FALSE)

  expect_equal(
    as.vector(terra::values(radians)),
    as.vector(terra::values(degrees)) * pi / 180,
    tolerance = 1e-6
  )
})

test_that("get_slope() supports degrees", {
  skip_on_os("mac")

  dem <- make_dem(
    values = matrix(c(
      10, 40, 80,
      10, 40, 80,
      10, 40, 80
    ), nrow = 3, byrow = TRUE)
  )

  radians <- get_slope(dem = dem, agg = 1, unit = "radians", verbose = FALSE)
  degrees <- get_slope(dem = dem, agg = 1, unit = "degrees", verbose = FALSE)

  expect_equal(
    as.vector(terra::values(degrees)),
    as.vector(terra::values(radians)) * 180 / pi,
    tolerance = 1e-6
  )
})

test_that("get_slope() rejects invalid unit", {
  dem <- make_dem()

  expect_error(
    get_slope(dem = dem, unit = "meter"),
    "'arg' should be one of"
  )
})

test_that("get_slope() aggregates DEM when resolution is smaller than agg", {
  skip_on_os("mac")

  dem <- make_dem(n = 10, res = 1, values = 1)

  slope <- get_slope(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(slope), c(5, 5))
})

test_that("get_slope() does not aggregate DEM when resolution is already large enough", {
  skip_on_os("mac")

  dem <- make_dem(n = 2, res = 10, values = 1)

  slope <- get_slope(dem = dem, agg = 5, verbose = FALSE)

  expect_equal(terra::res(slope), c(10, 10))
})


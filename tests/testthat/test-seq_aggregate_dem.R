test_that("seq_aggregate_dem returns input raster when agg is NULL", {

  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )

  out <- seq_aggregate_dem(r, agg = NULL, verbose = FALSE)

  expect_s4_class(out, "SpatRaster")
  expect_equal(terra::ncell(out), terra::ncell(r))
})

test_that("seq_aggregate_dem fails when dem is missing", {

  expect_error(
    seq_aggregate_dem(),
    "dem"
  )
})

test_that("seq_aggregate_dem fails with invalid dem", {

  expect_error(
    seq_aggregate_dem(1),
    "SpatRaster"
  )
})

test_that("seq_aggregate_dem fails with invalid agg", {

  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )

  expect_error(
    seq_aggregate_dem(r, agg = -1),
    "agg"
  )

  expect_error(
    seq_aggregate_dem(r, agg = NA),
    "agg"
  )

  expect_error(
    seq_aggregate_dem(r, agg = c(5, 10)),
    "agg"
  )
})

test_that("seq_aggregate_dem fails with invalid verbose", {

  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )

  expect_error(
    seq_aggregate_dem(r, verbose = NA),
    "verbose"
  )

  expect_error(
    seq_aggregate_dem(r, verbose = 1),
    "verbose"
  )
})

test_that("seq_aggregate_dem does not aggregate when resolution is already coarse enough", {

  r <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100
  )

  out <- seq_aggregate_dem(
    r,
    agg = 5,
    verbose = FALSE
  )

  expect_equal(
    terra::ncell(out),
    terra::ncell(r)
  )

  expect_equal(
    terra::res(out),
    terra::res(r)
  )
})

test_that("seq_aggregate_dem aggregates raster to coarser resolution", {

  r <- terra::rast(
    nrows = 100,
    ncols = 100,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100
  )

  terra::values(r) <- 1

  out <- seq_aggregate_dem(
    r,
    agg = 5,
    verbose = FALSE
  )

  expect_equal(
    terra::res(out),
    c(5, 5)
  )

  expect_equal(
    terra::ncell(out),
    400
  )
})

test_that("seq_aggregate_dem preserves mean values after aggregation", {

  r <- terra::rast(
    nrows = 100,
    ncols = 100,
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100
  )

  terra::values(r) <- 10

  out <- seq_aggregate_dem(
    r,
    agg = 5,
    verbose = FALSE
  )

  expect_true(
    all(terra::values(out) == 10)
  )
})

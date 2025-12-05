test_that("get_chm() errors when mixing x with dem/dsm", {

  pt <- sf::st_sfc(sf::st_point(c(1,1)), crs = 2154)

  expect_error(
    get_chm(x = pt, dem = terra::rast(), dsm = terra::rast()),
    "cannot be used together"
  )
})

test_that("get_chm() errors when dem or dsm is missing in manual mode", {

  fake_dem <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)
  fake_dsm <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)

  expect_error(get_chm(dem = fake_dem), "must be provided")
  expect_error(get_chm(dsm = fake_dsm), "must be provided")
})

test_that("get_chm() calls get_dem() and get_dsm() when x is provided", {

  pt <- sf::st_sfc(sf::st_point(c(1,1)), crs = 2154)

  fake_dem <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)
  terra::values(fake_dem) <- 1

  fake_dsm <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)
  terra::values(fake_dsm) <- 2

  tracker <- new.env(parent = emptyenv())
  tracker$dem_called <- FALSE
  tracker$dsm_called <- FALSE

  local_mocked_bindings(
    get_dem = function(x, ...) { tracker$dem_called <- TRUE; fake_dem },
    get_dsm = function(x, ...) { tracker$dsm_called <- TRUE; fake_dsm }
  )

  chm <- get_chm(x = pt, verbose = FALSE)

  expect_true(tracker$dem_called)
  expect_true(tracker$dsm_called)
  expect_s4_class(chm, "SpatRaster")
  expect_equal(as.numeric(chm[]), 1) # CHM = DSM_DEM = 2 - 1 = 1
  expect_shape(chm, dim = c(1, 1, 1))
  expect_named(chm, "chm")
})

test_that("get_chm() computes DSM − DEM in manual mode", {

  dem <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)
  dsm <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)

  terra::values(dem) <- 1
  terra::values(dsm) <- 2

  chm <- get_chm(dem = dem, dsm = dsm)

  expect_s4_class(chm, "SpatRaster")
  expect_equal(as.numeric(chm[]), 1)
})

test_that("get_chm() clamps negative values in DEM/DSM to 0", {

  dem <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)
  dsm <- terra::rast(nrows=1, ncols=1, xmin=0, xmax=1, ymin=0, ymax=1)

  terra::values(dem) <- -5
  terra::values(dsm) <- -1

  chm <- get_chm(dem = dem, dsm = dsm)

  expect_equal(as.numeric(chm[]), 0)   # dsm=0, dem=0 → CHM=0
})

test_that("get_chm() sets CHM below minmax[1] to NA", {

  dem <- terra::rast(nrows=1, ncols=1)
  dsm <- terra::rast(nrows=1, ncols=1)

  terra::values(dem) <- 10
  terra::values(dsm) <- 11   # CHM = 1

  chm <- get_chm(dem = dem, dsm = dsm, minmax = c(5, 50))

  expect_true(is.na(as.numeric(chm[])))
})

test_that("get_chm() caps CHM above minmax[2]", {

  dem <- terra::rast(nrows=1, ncols=1)
  dsm <- terra::rast(nrows=1, ncols=1)

  terra::values(dem) <- 0
  terra::values(dsm) <- 100   # CHM = 100

  chm <- get_chm(dem = dem, dsm = dsm, minmax = c(0, 50))

  expect_equal(as.numeric(chm[]), 50)
})

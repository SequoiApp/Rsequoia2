make_test_raster_file <- function(x = Rsequoia2:::seq_poly, crs = sf::st_crs(x)$wkt) {
  f <- tempfile(fileext = ".tif")

  x <- sf::st_transform(x, sf::st_crs(crs))
  bb <- sf::st_bbox(x)

  r <- terra::rast(
    nrows = 20,
    ncols = 20,
    xmin = bb["xmin"] - 500,
    xmax = bb["xmax"] + 500,
    ymin = bb["ymin"] - 500,
    ymax = bb["ymax"] + 500,
    crs = crs
  )

  terra::values(r) <- seq_len(terra::ncell(r))
  terra::writeRaster(r, f, overwrite = TRUE)

  f
}

test_that("get_lidar() aborts when cleaned envelope has no valid polygon", {

  download_called <- FALSE
  testthat::local_mocked_bindings(
    seq_envelope = function(...) Rsequoia2:::seq_empty,
    download_lidar = function(...) {
      download_called <<- TRUE
      tempfile(fileext = ".tif")
    },
    .package = "Rsequoia2"
  )

  expect_error(
    get_lidar(Rsequoia2:::seq_poly, key = "mnt", verbose = FALSE),
    "has no valid polygon geometry after cleaning"
  )

  expect_false(download_called)
})

test_that("get_lidar() returns a named SpatRaster for each lidar key", {
  x <- Rsequoia2:::seq_poly
  raster_file <- make_test_raster_file(x)

  testthat::local_mocked_bindings(
    seq_envelope = function(x, ...) x,
    download_lidar = function(...) raster_file,
    .package = "Rsequoia2"
  )

  keys <- c("mnt", "mns", "mnh")
  res <- lapply(
    keys,
    function(key) get_lidar(x = x, key = key, verbose = FALSE)
  )

  expect_true(all(vapply(res, inherits, logical(1), "SpatRaster")))

  expect_identical(
    vapply(res, names, character(1)),
    paste0(keys, "_lidar")
  )
})

test_that("get_lidar() crops raster to input polygon extent", {
  x <- Rsequoia2:::seq_poly
  raster_file <- make_test_raster_file(x)

  testthat::local_mocked_bindings(
    seq_envelope = function(x, ...) x,
    download_lidar = function(...) raster_file,
    .package = "Rsequoia2"
  )

  r <- get_lidar(x = x, key = "mnt", buffer = 0, verbose = FALSE)

  bb <- sf::st_bbox(sf::st_transform(x, terra::crs(r)))
  ext <- terra::ext(r)

  expect_lte(ext[1], bb["xmax"])
  expect_gte(ext[2], bb["xmin"])
  expect_lte(ext[3], bb["ymax"])
  expect_gte(ext[4], bb["ymin"])
})

test_that("get_lidar() reprojects raster to requested CRS", {
  x <- Rsequoia2:::seq_poly

  raster_file <- make_test_raster_file(
    x = sf::st_transform(x, 3857),
    crs = "EPSG:3857"
  )

  testthat::local_mocked_bindings(
    seq_envelope = function(x, ...) x,
    download_lidar = function(...) raster_file,
    .package = "Rsequoia2"
  )

  r <- get_lidar(x = x, key = "mnt", crs = 2154, verbose = FALSE)

  expect_s4_class(r, "SpatRaster")
  expect_true(terra::same.crs(r, "EPSG:2154"))
})


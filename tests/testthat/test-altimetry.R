make_altimetry_raster <- function(name) {
  r <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:2154"
  )
  terra::values(r) <- 1:4
  names(r) <- name
  r
}


make_altimetry_area <- function() {
  sf::st_as_sf(
    data.frame(x = 1, y = 1),
    coords = c("x", "y"),
    crs = 2154
  )
}


test_that("altimetry fetcher uses LiDAR when it is available", {
  local_mocked_bindings(
    get_lidar = function(x, key, ...) {
      make_altimetry_raster(paste0(key, "_lidar"))
    },
    get_rge = function(...) {
      cli::cli_abort("RGE should not be called")
    },
    seq_write = function(x, key, ...) key,
    .package = "Rsequoia2"
  )

  paths <- Rsequoia2:::.altimetry_fetcher(
    x = make_altimetry_area(),
    dirname = tempdir(),
    key = "mnt",
    verbose = FALSE
  )

  expect_identical(paths$mnt, "r.alt.mnt.lidar")
})


test_that("altimetry fetcher falls back to RGE when LiDAR is unavailable", {
  local_mocked_bindings(
    get_lidar = function(...) {
      cli::cli_abort("LiDAR unavailable")
    },
    get_rge = function(x, key, ...) {
      make_altimetry_raster(paste0(key, "_rge"))
    },
    seq_write = function(x, key, ...) key,
    .package = "Rsequoia2"
  )

  paths <- Rsequoia2:::.altimetry_fetcher(
    x = make_altimetry_area(),
    dirname = tempdir(),
    key = "mnt",
    verbose = FALSE
  )

  expect_identical(paths$mnt, "r.alt.mnt.rge")
})

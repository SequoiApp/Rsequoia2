test_that("fetch_climate() forwards arguments", {

  tracker <- list(args = NULL)

  local_mocked_bindings(
    .climate_fetcher = function(
    x, buffer, cache, dirname, id, overwrite, verbose
    ) {
      tracker$args <<- list(
        x = x,
        buffer = buffer,
        cache = cache,
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )

      "CLIM_TYPE_poly.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  path <- fetch_climate(
    x = x,
    buffer = 250,
    dirname = "output",
    cache = "climate_cache",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(path, "CLIM_TYPE_poly.gpkg")
  expect_identical(tracker$args$x, x)
  expect_identical(tracker$args$buffer, 250)
  expect_identical(tracker$args$cache, "climate_cache")
  expect_identical(tracker$args$dirname, "output")
  expect_null(tracker$args$id)
  expect_identical(tracker$args$overwrite, TRUE)
  expect_identical(tracker$args$verbose, FALSE)
})


test_that("fetch_climate() uses default arguments", {

  tracker <- list(args = NULL)

  local_mocked_bindings(
    .climate_fetcher = function(
    x, buffer, cache, dirname, id, overwrite, verbose
    ) {
      tracker$args <<- list(
        buffer = buffer,
        cache = cache,
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )

      "CLIM_TYPE_poly.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  fetch_climate(
    x = x,
    dirname = "output"
  )

  expect_identical(tracker$args$buffer, 100)
  expect_null(tracker$args$cache)
  expect_identical(tracker$args$dirname, "output")
  expect_null(tracker$args$id)
  expect_identical(tracker$args$overwrite, FALSE)
  expect_identical(tracker$args$verbose, TRUE)
})


test_that("fetch_climate() returns NULL when no climate is found", {

  local_mocked_bindings(
    .climate_fetcher = function(...) NULL
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- fetch_climate(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_null(result)
})

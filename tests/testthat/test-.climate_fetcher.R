test_that(".climate_fetcher() fetches and writes climate data", {

  climate <- data.frame(label = "climat océanique")
  write_args <- NULL
  fetch_args <- NULL

  testthat::local_mocked_bindings(
    get_climate = function(x, buffer, cache, overwrite, verbose) {
      fetch_args <<- list(
        x = x,
        buffer = buffer,
        cache = cache,
        overwrite = overwrite,
        verbose = verbose
      )
      "raster"
    },
    .climate_transformer = function(x, climate) {
      climate_result <- climate
      climate_result
    },
    seq_write = function(x, key, dirname, id, verbose, overwrite) {
      write_args <<- list(
        x = x,
        key = key,
        dirname = dirname,
        id = id,
        verbose = verbose,
        overwrite = overwrite
      )
      "climate.gpkg"
    },
    .package = "Rsequoia2"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- .climate_fetcher(
    x = x,
    buffer = 50,
    cache = "cache",
    dirname = "output",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(result, "climate.gpkg")
  expect_identical(fetch_args$buffer, 50)
  expect_identical(fetch_args$cache, "cache")
  expect_identical(fetch_args$overwrite, TRUE)
  expect_identical(write_args$key, "v.clim.type.poly")
  expect_identical(write_args$dirname, "output")
  expect_identical(write_args$overwrite, TRUE)
})


test_that(".climate_fetcher() adds the project identifier", {

  climate <- data.frame(label = "climat océanique")
  written <- NULL

  testthat::local_mocked_bindings(
    get_climate = function(...) "raster",
    .climate_transformer = function(...) climate,
    seq_field = function(...) list(name = "identifier"),
    seq_write = function(x, ...) {
      written <<- x
      "climate.gpkg"
    },
    .package = "Rsequoia2"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  .climate_fetcher(
    x = x,
    dirname = "output",
    id = "project-01",
    verbose = FALSE
  )

  expect_identical(written$identifier, "project-01")
})


test_that(".climate_fetcher() returns NULL when no climate is found", {

  testthat::local_mocked_bindings(
    get_climate = function(...) "raster",
    .climate_transformer = function(...) NULL,
    seq_write = function(...) {
      cli::cli_abort("seq_write() should not be called.")
    },
    .package = "Rsequoia2"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- .climate_fetcher(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_null(result)
})

test_that("fetch_guides() forwards arguments", {

  tracker <- list(args = NULL)

  testthat::local_mocked_bindings(
    .guides_fetcher = function(
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

      "guides.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  path <- fetch_guides(
    x = x,
    buffer = 250,
    dirname = "output",
    cache = "guides_cache",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(path, "guides.gpkg")
  expect_identical(tracker$args$x, x)
  expect_identical(tracker$args$buffer, 250)
  expect_identical(tracker$args$cache, "guides_cache")
  expect_identical(tracker$args$dirname, "output")
  expect_null(tracker$args$id)
  expect_identical(tracker$args$overwrite, TRUE)
  expect_identical(tracker$args$verbose, FALSE)
})


test_that("fetch_guides() uses default arguments", {

  tracker <- list(args = NULL)

  testthat::local_mocked_bindings(
    .guides_fetcher = function(
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

      "guides.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  fetch_guides(
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


test_that("fetch_guides() returns NULL when no guides are found", {

  testthat::local_mocked_bindings(
    .guides_fetcher = function(...) NULL
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- fetch_guides(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_null(result)
})

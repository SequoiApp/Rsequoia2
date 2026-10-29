test_that("fetch_forest() forwards arguments", {

  tracker <- list(args = NULL)

  local_mocked_bindings(
    .forest_fetcher = function(
    x, dirname, id = NULL, buffer, verbose, overwrite
    ) {
      tracker$args <<- list(
        x = x,
        dirname = dirname,
        id = id,
        buffer = buffer,
        verbose = verbose,
        overwrite = overwrite
      )

      "BD_FORET_V2_poly.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  path <- fetch_forest(
    x = x,
    buffer = 250,
    dirname = "output",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(path, "BD_FORET_V2_poly.gpkg")
  expect_identical(tracker$args$x, x)
  expect_identical(tracker$args$buffer, 250)
  expect_identical(tracker$args$dirname, "output")
  expect_null(tracker$args$id)
  expect_identical(tracker$args$overwrite, TRUE)
  expect_identical(tracker$args$verbose, FALSE)

})


test_that("fetch_forest() uses default arguments", {

  tracker <- list(args = NULL)

  local_mocked_bindings(
    .forest_fetcher = function(
    x, dirname, id = NULL, buffer, verbose, overwrite
    ) {
      tracker$args <<- list(
        buffer = buffer,
        dirname = dirname,
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )

      "BD_FORET_V2_poly.gpkg"
    }
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  fetch_forest(
    x = x,
    dirname = "output"
  )

  expect_identical(tracker$args$buffer, 1000)
  expect_identical(tracker$args$dirname, "output")
  expect_null(tracker$args$id)
  expect_identical(tracker$args$overwrite, FALSE)
  expect_identical(tracker$args$verbose, TRUE)

})


test_that("fetch_forest() returns NULL when no forest is found", {

  local_mocked_bindings(
    .forest_fetcher = function(...) NULL
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- fetch_forest(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_null(result)

})

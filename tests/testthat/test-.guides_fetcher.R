test_that(".guides_fetcher() fetches and writes guides data", {

  guides <- list(
    guides = sf::st_sf(id = 1L, geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), crs = 2154
    )),
    guides_urls = data.frame(guide_id = 1L, url = "guide.pdf"),
    stations = data.frame(guide_id = 1L, station = "Station 1")
  )

  fetch_args <- NULL
  write_args <- NULL
  written_layers <- character()
  pdf_args <- NULL

  testthat::local_mocked_bindings(
    get_guides = function(x, buffer, cache, overwrite, verbose) {
      fetch_args <<- list(
        x = x,
        buffer = buffer,
        cache = cache,
        overwrite = overwrite,
        verbose = verbose
      )
      guides
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
      "guides.gpkg"
    },
    get_guides_pdf = function(guides_urls, dirname, overwrite, verbose) {
      pdf_args <<- list(
        guides_urls = guides_urls,
        dirname = dirname,
        overwrite = overwrite,
        verbose = verbose
      )
      invisible(dirname)
    },
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    write_sf = function(x, dsn, layer, delete_layer) {
      written_layers <<- c(written_layers, layer)
      invisible(dsn)
    },
    .package = "sf"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- .guides_fetcher(
    x = x,
    buffer = 50,
    cache = "cache",
    dirname = "output",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(result, "guides.gpkg")

  expect_identical(fetch_args$x, x)
  expect_identical(fetch_args$buffer, 50)
  expect_identical(fetch_args$cache, "cache")
  expect_identical(fetch_args$overwrite, TRUE)
  expect_identical(fetch_args$verbose, FALSE)

  expect_identical(write_args$x, guides$guides)
  expect_identical(write_args$key, "v.sol.guides.poly")
  expect_identical(write_args$dirname, "output")
  expect_identical(write_args$id, NULL)
  expect_identical(write_args$verbose, FALSE)
  expect_identical(write_args$overwrite, TRUE)

  expect_setequal(written_layers, c("guides_urls", "stations"))

  expect_identical(pdf_args$guides_urls, guides$guides_urls)
  expect_identical(pdf_args$dirname, "output")
  expect_identical(pdf_args$overwrite, TRUE)
  expect_identical(pdf_args$verbose, FALSE)
})

test_that(".guides_fetcher() adds the project identifier", {

  guides <- list(
    guides = sf::st_sf(
      id = 1L,
      geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
    ),
    guides_urls = data.frame(guide_id = 1L, url = "guide.pdf"),
    stations = data.frame(guide_id = 1L, station = "Station 1")
  )

  written <- NULL

  testthat::local_mocked_bindings(
    get_guides = function(...) guides,
    seq_field = function(...) list(name = "identifier"),
    seq_write = function(x, ...) {
      written <<- x
      "guides.gpkg"
    },
    get_guides_pdf = function(...) invisible(NULL),
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    write_sf = function(...) invisible(NULL),
    .package = "sf"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  .guides_fetcher(
    x = x,
    dirname = "output",
    id = "project-01",
    verbose = FALSE
  )

  expect_identical(written$identifier, "project-01")
})

test_that(".guides_fetcher() returns NULL when no guides are found", {

  testthat::local_mocked_bindings(
    get_guides = function(...) NULL,
    seq_write = function(...) {
      cli::cli_abort("seq_write() should not be called.")
    },
    get_guides_pdf = function(...) {
      cli::cli_abort("get_guides_pdf() should not be called.")
    },
    .package = "Rsequoia2"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  result <- .guides_fetcher(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_null(result)
})

test_that(".guides_fetcher() uses default arguments", {

  guides <- list(
    guides = sf::st_sf(
      id = 1L,
      geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
    ),
    guides_urls = data.frame(guide_id = 1L, url = "guide.pdf"),
    stations = data.frame(guide_id = 1L, station = "Station 1")
  )

  fetch_args <- NULL
  write_args <- NULL

  testthat::local_mocked_bindings(
    get_guides = function(x, buffer, cache, overwrite, verbose) {
      fetch_args <<- list(
        buffer = buffer,
        cache = cache,
        overwrite = overwrite,
        verbose = verbose
      )
      guides
    },
    seq_write = function(x, key, dirname, id, verbose, overwrite) {
      write_args <<- list(
        id = id,
        overwrite = overwrite,
        verbose = verbose
      )
      "guides.gpkg"
    },
    get_guides_pdf = function(...) invisible(NULL),
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    write_sf = function(...) invisible(NULL),
    .package = "sf"
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  .guides_fetcher(
    x = x,
    dirname = "output",
    verbose = FALSE
  )

  expect_identical(fetch_args$buffer, 100)
  expect_null(fetch_args$cache)
  expect_identical(fetch_args$overwrite, FALSE)
  expect_identical(fetch_args$verbose, FALSE)

  expect_null(write_args$id)
  expect_identical(write_args$overwrite, FALSE)
  expect_identical(write_args$verbose, FALSE)
})

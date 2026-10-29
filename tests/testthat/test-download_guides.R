test_that("download_guides downloads the GeoPackage", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeBin(as.raw(c(1, 2, 3)), destfile)
      0L
    },
    .package = "utils"
  )

  path <- download_guides(
    cache = tmp,
    verbose = FALSE
  )

  expect_type(path, "character")
  expect_identical(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    normalizePath(
      file.path(tmp, "gs.gpkg"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})


test_that("download_guides creates the cache directory", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeBin(as.raw(c(1, 2, 3)), destfile)
      0L
    },
    .package = "utils"
  )

  download_guides(
    cache = file.path(tmp, "cache"),
    verbose = FALSE
  )

  expect_true(dir.exists(file.path(tmp, "cache")))
  expect_true(file.exists(file.path(tmp, "cache", "gs.gpkg")))
})


test_that("download_guides reuses an existing file", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- file.path(tmp, "gs.gpkg")
  writeBin(as.raw(c(1, 2, 3)), path)

  testthat::local_mocked_bindings(
    download.file = function(...) {
      stop("Download should not be called")
    },
    .package = "utils"
  )

  result <- download_guides(
    cache = tmp,
    verbose = FALSE
  )

  expect_identical(
    normalizePath(result, winslash = "/", mustWork = TRUE),
    normalizePath(path, winslash = "/", mustWork = TRUE)
  )
})


test_that("download_guides overwrites an existing file", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- file.path(tmp, "gs.gpkg")
  writeBin(as.raw(1), path)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeBin(as.raw(c(1, 2, 3)), destfile)
      0L
    },
    .package = "utils"
  )

  download_guides(
    cache = tmp,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_equal(file.info(path)$size, 3)
})


test_that("download_guides handles download failures", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      1L
    },
    .package = "utils"
  )

  expect_error(
    download_guides(
      cache = tmp,
      verbose = FALSE
    )
  )
})


test_that("download_guides handles missing downloaded files", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      0L
    },
    .package = "utils"
  )

  expect_error(
    download_guides(
      cache = tmp,
      verbose = FALSE
    )
  )
})


test_that("download_guides handles empty downloaded files", {

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.create(destfile)
      0L
    },
    .package = "utils"
  )

  expect_error(
    download_guides(
      cache = tmp,
      verbose = FALSE
    )
  )
})


test_that("download_guides validates arguments", {

  expect_error(
    download_guides(cache = 1)
  )

  expect_error(
    download_guides(cache = "")
  )

  expect_error(
    download_guides(cache = c("a", "b"))
  )

  expect_error(
    download_guides(overwrite = 1)
  )

  expect_error(
    download_guides(verbose = 1)
  )
})


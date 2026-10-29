test_that("download_climate() downloads when file is missing", {

  climate_cache <- file.path(tempdir(), "climate_download")
  dir.create(climate_cache, showWarnings = FALSE)
  on.exit(unlink(climate_cache, recursive = TRUE, force = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      tracker$called <- TRUE
      writeBin(as.raw(1:4), destfile)
      0L
    },
    .package = "utils"
  )

  out <- download_climate(
    cache = climate_cache,
    verbose = FALSE
  )

  expect_true(tracker$called)
  expect_true(file.exists(out))
  expect_identical(basename(out), "TYPO_RGF93.tif")
  expect_identical(
    normalizePath(dirname(out), winslash = "/", mustWork = TRUE),
    normalizePath(climate_cache, winslash = "/", mustWork = TRUE)
  )
})

test_that("download_climate() skips download when file already exists", {

  climate_cache <- file.path(tempdir(), "climate_download")
  dir.create(climate_cache, showWarnings = FALSE)
  on.exit(unlink(climate_cache, recursive = TRUE, force = TRUE), add = TRUE)

  tif_path <- file.path(climate_cache, "TYPO_RGF93.tif")
  writeBin(as.raw(1:4), tif_path)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      tracker$called <- TRUE
      writeBin(as.raw(5:8), destfile)
      0L
    },
    .package = "utils"
  )

  out <- download_climate(
    cache = climate_cache,
    verbose = FALSE
  )

  expect_false(tracker$called)
  expect_identical(out, tif_path)
  expect_true(file.exists(out))
  expect_identical(readBin(out, "raw", n = 4L), as.raw(1:4))
})

test_that("download_climate() errors when download fails", {

  climate_cache <- file.path(tempdir(), "climate_download")
  dir.create(climate_cache, showWarnings = FALSE)
  on.exit(unlink(climate_cache, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      1L
    },
    .package = "utils"
  )

  expect_error(
    download_climate(
      cache = climate_cache,
      verbose = FALSE
    ),
    "Download failed"
  )
})

test_that("download_climate() errors when downloaded file is missing", {

  climate_cache <- file.path(tempdir(), "climate_download")
  dir.create(climate_cache, showWarnings = FALSE)
  on.exit(unlink(climate_cache, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      0L
    },
    .package = "utils"
  )

  expect_error(
    download_climate(
      cache = climate_cache,
      verbose = FALSE
    ),
    "Download failed"
  )
})

test_that("download_climate() validates arguments", {

  expect_error(
    download_climate(cache = "", verbose = FALSE)
  )

  expect_error(
    download_climate(cache = NA_character_, verbose = FALSE)
  )

  expect_error(
    download_climate(cache = c("a", "b"), verbose = FALSE)
  )

  expect_error(
    download_climate(cache = tempdir(), overwrite = NA, verbose = FALSE)
  )

  expect_error(
    download_climate(cache = tempdir(), overwrite = 1, verbose = FALSE)
  )

  expect_error(
    download_climate(cache = tempdir(), verbose = NA)
  )

  expect_error(
    download_climate(cache = tempdir(), verbose = 1)
  )
})

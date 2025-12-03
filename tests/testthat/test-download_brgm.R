test_that("download_brgm() downloads when file is missing", {

  d <- tempdir()

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet){
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  zip_path <- download_brgm(dep = "29", cache = d, verbose = FALSE)
  on.exit(unlink(zip_path))

  expect_true(tracker$called)
  expect_true(file.exists(zip_path))
  expect_match(zip_path, "GEO050K_HARM_029.zip")

})

test_that("download_brgm() skips download when file already exists", {

  d <- tempdir()

  zip_path <- file.path(d, "GEO050K_HARM_029.zip")
  file.create(zip_path)
  on.exit(unlink(zip_path))

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet){
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  out <- download_brgm(dep = "29", cache = d, verbose = FALSE)
  on.exit(unlink(out))

  expect_false(tracker$called)
  expect_equal(out, zip_path)
  expect_true(file.exists(out))
})

test_that("download_brgm() errors on invalid dep", {

  expect_error(download_brgm(dep = 1:2), "must contain exactly one element")

  expect_error(download_brgm(dep = 123), "Invalid department code")
  expect_error(download_brgm(dep = "A1"), "Invalid department code")
})

test_that("download_brgm() respects cache argument", {

  cache <- tempdir()

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  zip_path <- download_brgm("29", cache = cache, verbose = FALSE)
  on.exit(unlink(zip_path))

  expect_identical(normalizePath(dirname(zip_path)), normalizePath(cache))
})

test_that("download_brgm() builds correct ZIP name", {

  cache <- tempdir()

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  zip_path <- download_brgm(8, cache = cache, verbose = FALSE)
  on.exit(unlink(zip_path))

  expect_match(basename(zip_path), "^GEO050K_HARM_008\\.zip$")
})

test_that("download_brgm() emits messages when verbose = TRUE", {

  cache <- tempdir()

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  expect_message(
    zip_path <- download_brgm("29", cache = cache, verbose = TRUE),
    "Downloading BRGM dataset for dep"
  ) |> suppressMessages()
  on.exit(unlink(zip_path))
})

test_that("download_brgm() overwrites existing ZIP when overwrite=TRUE", {

  d <- tempdir()
  existing <- file.path(d, "GEO050K_HARM_029.zip")
  file.create(existing)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  # Call overwrite = TRUE
  out <- download_brgm("29", cache = d, verbose = FALSE, overwrite = TRUE)

  expect_true(tracker$called)
  expect_true(file.exists(out))
})


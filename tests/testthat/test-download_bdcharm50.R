test_that("download_bdcharm50() downloads when file is missing", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    get_bdcharm50_url = function(...) {
      stats::setNames(
        "http://example.com/GEO050K_HARM_029.zip",
        "GEO050K_HARM_029.zip"
      )
    },
    .package = "Rsequoia2"
  )

  zip_path <- download_bdcharm50(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_true(tracker$called)
  expect_true(file.exists(zip_path))
  expect_match(basename(zip_path), "^GEO050K_HARM_029\\.zip$")
})

test_that("download_bdcharm50() skips download when file already exists", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  zip_path <- file.path(brgm_cache, "GEO050K_HARM_029.zip")
  file.create(zip_path)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    get_bdcharm50_url = function(...) {
      stats::setNames(
        "http://example.com/GEO050K_HARM_029.zip",
        "GEO050K_HARM_029.zip"
      )
    },
    .package = "Rsequoia2"
  )

  out <- download_bdcharm50(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_false(tracker$called)
  expect_equal(unname(out), zip_path)
  expect_true(file.exists(out))
})


test_that("download_bdcharm50() overwrites existing ZIP when overwrite = TRUE", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  zip_path <- file.path(brgm_cache, "GEO050K_HARM_029.zip")
  file.create(zip_path)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    get_bdcharm50_url = function(...) {
      stats::setNames(
        "http://example.com/GEO050K_HARM_029.zip",
        "GEO050K_HARM_029.zip"
      )
    },
    .package = "Rsequoia2"
  )

  out <- download_bdcharm50(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE,
    overwrite = TRUE
  )

  expect_true(tracker$called)
  expect_true(file.exists(out))
})


test_that("download_bdcharm50() supports several departments", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      file.create(destfile)
    },
    .package = "curl"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(
        DEP = c("08", "29"),
        NCC_DEP = c("ARDENNES", "FINISTERE")
      ))
    },
    get_bdcharm50_url = function(...) {
      stats::setNames(
        c(
          "http://example.com/GEO050K_HARM_008.zip",
          "http://example.com/GEO050K_HARM_029.zip"
        ),
        c(
          "GEO050K_HARM_008.zip",
          "GEO050K_HARM_029.zip"
        )
      )
    },
    .package = "Rsequoia2"
  )

  out <- download_bdcharm50(
    dep = c(8, 29),
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_named(out, c("08", "29"))
  expect_length(out, 2)
  expect_true(all(file.exists(out)))
  expect_match(basename(out[["08"]]), "^GEO050K_HARM_008\\.zip$")
  expect_match(basename(out[["29"]]), "^GEO050K_HARM_029\\.zip$")
})


test_that("download_bdcharm50() respects cache argument", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      file.create(destfile)
    },
    .package = "curl"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    get_bdcharm50_url = function(...) {
      stats::setNames(
        "http://example.com/GEO050K_HARM_029.zip",
        "GEO050K_HARM_029.zip"
      )
    },
    .package = "Rsequoia2"
  )

  out <- download_bdcharm50(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_identical(
    normalizePath(dirname(out), winslash = "/"),
    normalizePath(brgm_cache, winslash = "/")
  )
})


test_that("download_bdcharm50() errors on invalid department", {

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    .package = "Rsequoia2"
  )

  expect_error(
    download_bdcharm50(dep = 123, verbose = FALSE),
    "Invalid department code"
  )

  expect_error(
    download_bdcharm50(dep = "A1", verbose = FALSE),
    "Invalid department code"
  )
})

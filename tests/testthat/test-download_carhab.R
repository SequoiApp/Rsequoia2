test_that("download_carhab() downloads when file is missing", {

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
    .package = "Rsequoia2"
  )

  zip_path <- download_carhab(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_true(tracker$called)
  expect_true(file.exists(zip_path))
  expect_match(basename(zip_path), "^CARHAB_29_FINISTERE\\.zip$")
})


test_that("download_carhab() skips download when file already exists", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  zip_path <- file.path(brgm_cache, "CARHAB_29_FINISTERE.zip")
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
    .package = "Rsequoia2"
  )

  out <- download_carhab(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_false(tracker$called)
  expect_equal(unname(out), zip_path)
  expect_true(file.exists(out))
})


test_that("download_carhab() overwrites existing ZIP when overwrite = TRUE", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache, showWarnings = FALSE)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  zip_path <- file.path(brgm_cache, "CARHAB_29_FINISTERE.zip")
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
    .package = "Rsequoia2"
  )

  out <- download_carhab(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE,
    overwrite = TRUE
  )

  expect_true(tracker$called)
  expect_true(file.exists(out))
})


test_that("download_carhab() supports several departments", {

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
    .package = "Rsequoia2"
  )

  out <- download_carhab(
    dep = c(8, 29),
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_named(out, c("08", "29"))
  expect_length(out, 2)
  expect_true(all(file.exists(out)))
  expect_match(basename(out[["08"]]), "^CARHAB_08_ARDENNES\\.zip$")
  expect_match(basename(out[["29"]]), "^CARHAB_29_FINISTERE\\.zip$")
})


test_that("download_carhab() respects cache argument", {

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
    .package = "Rsequoia2"
  )

  out <- download_carhab(
    dep = "29",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_identical(
    normalizePath(dirname(out), winslash = "/"),
    normalizePath(brgm_cache, winslash = "/")
  )
})


test_that("download_carhab() handles department names with spaces", {

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
        DEP = c("35", "22"),
        NCC_DEP = c("ILLE ET VILAINE", "COTES D ARMOR")
      ))
    },
    .package = "Rsequoia2"
  )

  out_35 <- download_carhab(
    dep = "35",
    cache = brgm_cache,
    verbose = FALSE
  )

  out_22 <- download_carhab(
    dep = "22",
    cache = brgm_cache,
    verbose = FALSE
  )

  expect_match(basename(out_35), "^CARHAB_35_ILLE-ET-VILAINE\\.zip$")
  expect_match(basename(out_22), "^CARHAB_22_COTES-D-ARMOR\\.zip$")
})


test_that("download_carhab() errors on invalid department", {

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    .package = "Rsequoia2"
  )

  expect_error(
    download_carhab(dep = 123, verbose = FALSE),
    "Invalid department code"
  )

  expect_error(
    download_carhab(dep = "A1", verbose = FALSE),
    "Invalid department code"
  )
})

test_that("download_brgm() downloads when file is missing", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet){
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  zip_path <- download_brgm(dep = "29", source = "carhab", cache = brgm_cache, verbose = FALSE)

  expect_true(tracker$called)
  expect_true(file.exists(zip_path))
  expect_match(zip_path, "CARHAB_29_FINISTERE")

})

test_that("download_brgm() properly change source", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet){
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  zip_path <- download_brgm(dep = "29", source = "bdcharm50", cache = brgm_cache, verbose = FALSE)

  expect_true(tracker$called)
  expect_true(file.exists(zip_path))
  expect_match(zip_path, "GEO050K_HARM_029")

})

test_that("download_brgm() skips download when file already exists", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  zip_path <- file.path(brgm_cache, "CARHAB_29_FINISTERE.zip")
  file.create(zip_path)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet){
      tracker$called <- TRUE
      file.create(destfile)
    },
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  out <- download_brgm(dep = "29", cache = brgm_cache, verbose = FALSE)

  expect_false(tracker$called)
  expect_equal(out, zip_path)
  expect_true(file.exists(out))
})

test_that("download_brgm() errors on invalid input", {

  expect_error(download_brgm(dep = 1, source = "bad_source"))

  expect_error(download_brgm(dep = 1:2), "must contain exactly one element")
  expect_error(download_brgm(dep = 1, source = 1:2), "must contain exactly one element")

  expect_error(download_brgm(dep = 123), "Invalid department code")
  expect_error(download_brgm(dep = "A1"), "Invalid department code")
})

test_that("download_brgm() respects cache argument", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  zip_path <- download_brgm("29", cache = brgm_cache, verbose = FALSE)

  expect_identical(normalizePath(dirname(zip_path)), normalizePath(brgm_cache))
})

test_that("download_brgm() builds correct ZIP name", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "08", NCC_DEP = "ARDENNES"))
  )

  bdcharm50_path <- download_brgm(8, cache = brgm_cache, source = "bdcharm50", verbose = FALSE)
  carhab_path <- download_brgm(8, cache = brgm_cache, source = "carhab", verbose = FALSE)

  expect_match(basename(bdcharm50_path), "^GEO050K_HARM_008\\.zip$")
  expect_match(basename(carhab_path), "^CARHAB_08_ARDENNES\\.zip$")
})

test_that("download_brgm() handle ZIP name with space when carhab", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(
      DEP = c("35", "22"),
      NCC_DEP = c("ILLE ET VILAINE", "COTES D ARMOR")
      ))
  )

  carhab_path <- download_brgm(35, cache = brgm_cache, source = "carhab", verbose = FALSE)
  expect_match(basename(carhab_path), "35_ILLE-ET-VILAINE")

  carhab_path <- download_brgm(22, cache = brgm_cache, source = "carhab", verbose = FALSE)
  expect_match(basename(carhab_path), "22_COTES-D-ARMOR")

})

test_that("download_brgm() emits messages when verbose = TRUE", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  local_mocked_bindings(
    curl_download = function(url, destfile, quiet) file.create(destfile),
    .package = "curl"
  )

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  expect_message(
    zip_path <- download_brgm("29", cache = brgm_cache, verbose = TRUE),
    "Downloading BRGM dataset for dep"
  ) |> suppressMessages()
})

test_that("download_brgm() overwrites existing ZIP when overwrite=TRUE", {

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE))

  existing <- file.path(brgm_cache, "GEO050K_HARM_029.zip")
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

  local_mocked_bindings(
    get_cog = function(...) list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
  )

  # Call overwrite = TRUE
  out <- download_brgm("29", source = "bdcharm50", cache = brgm_cache, verbose = FALSE, overwrite = TRUE)

  expect_true(tracker$called)
  expect_true(file.exists(out))
})


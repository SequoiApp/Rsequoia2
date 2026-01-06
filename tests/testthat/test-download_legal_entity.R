test_that("download_legal_entity() downloads when file is missing", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    archive_extract = function(...){
      tracker$called <- TRUE
    },
    .package = "archive"
  )

  local_mocked_bindings(
    dg_get_dataset = function(...) {
      list(
        resource = data.frame(
          title = "parcelle_latest_years_should_be_1997",
          url = "this/is/an/url",
          format = "zip"
        )
      )
    }
  )

  zip_path <- download_legal_entity(cache = le_cache, verbose = FALSE)

  expect_true(tracker$called)
  expect_match(zip_path, "legal_entity")

})

test_that("download_legal_entity() does not redownload when cache is already populated", {

  le_cache <- file.path(tempdir(), "legal_entity_cached")
  fake_downloaded_dir <- "parcelle_latest_years_should_be_1997"

  dir.create(le_cache)
  dir.create(file.path(le_cache, fake_downloaded_dir))

  on.exit(unlink(c(le_cache, fake_downloaded_dir), recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- FALSE

  local_mocked_bindings(
    dg_get_dataset = function(...) {
      list(
        resource = data.frame(
          title  = fake_downloaded_dir,
          url    = "this/is/an/url",
          format = "zip"
        )
      )
    }
  )

  local_mocked_bindings(
    archive_extract = function(...) {
      tracker$called <- TRUE
    },
    .package = "archive"
  )

  download_legal_entity(cache = le_cache, verbose = FALSE)

  expect_false(tracker$called)
})

test_that("download_legal_entity() verbose arg works", {

  le_cache <- file.path(tempdir(), "legal_entity_verbose")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    dg_get_dataset = function(...) {
      list(
        resource = data.frame(
          title  = "parcelle_latest_years_should_be_1997",
          url    = "this/is/an/url",
          format = "zip"
        )
      )
    }
  )

  local_mocked_bindings(
    archive_extract = function(...) invisible(NULL),
    .package = "archive"
  )

  expect_message(
    download_legal_entity(cache = le_cache, verbose = TRUE),
    regexp = "Downloading legal entity datasets"
  ) |> suppressMessages()

  expect_message(
    download_legal_entity(cache = le_cache, verbose = TRUE),
    regexp = "Data available at"
  ) |> suppressMessages()

  expect_silent(
    download_legal_entity(cache = le_cache, verbose = FALSE)
  )
})

test_that("download_legal_entity() selects the latest year only", {

  le_cache <- file.path(tempdir(), "legal_entity_latest")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$urls <- character()

  local_mocked_bindings(
    dg_get_dataset = function(...) {
      list(
        resource = data.frame(
          title  = c("parcelle_2021.zip", "parcelle_2023.zip", "parcelle_2022.zip"),
          url    = c("url2021", "url2023", "url2022"),
          format = "zip"
        )
      )
    }
  )

  local_mocked_bindings(
    archive_extract = function(x, ...) {
      tracker$urls <- c(tracker$urls, x)
    },
    .package = "archive"
  )

  download_legal_entity(cache = le_cache, verbose = FALSE)

  expect_identical(tracker$urls, "url2023")
})

test_that("download_legal_entity() errors when no year is found", {

  local_mocked_bindings(
    dg_get_dataset = function(...) {
      list(
        resource = data.frame(
          title  = "parcelle_no_year.zip",
          url    = "url",
          format = "zip",
          stringsAsFactors = FALSE
        )
      )
    }
  )

  expect_error(
    download_legal_entity(cache = tempdir(), verbose = FALSE),
    "No valid year found in dataset for legal entity parcels."
  )
})

test_that("get_pedology_pdf() validates id_ucs", {

  expect_error(get_pedology_pdf(NULL), "must be a non-empty vector")
  expect_error(get_pedology_pdf(character(0)),"must be a non-empty vector")

})

test_that("get_pedology_pdf() downloads files", {

  cache <- file.path(tempdir(), "pedology_pdf")
  dir.create(cache, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  tracker <- list(called = 0)
  local_mocked_bindings(
    curl_download = function(url, destfile, ...){
      tracker$called <<- tracker$called + 1
      file.create(destfile)
    },
    .package = "curl"
  )

  ids <- c("A", "B")
  paths <- get_pedology_pdf(ids, dirname = cache, verbose = FALSE)

  expect_length(paths, 2)
  expect_true(all(file.exists(paths)))
  expect_named(paths, paste0("id_ucs_", ids))
  expect_equal(tracker$called, 2)

})

test_that("get_pedology_pdf() removes duplicate ids", {

  cache <- file.path(tempdir(), "pedology_pdf")
  dir.create(cache, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  tracker <- list(called = 0)

  local_mocked_bindings(
    curl_download = function(url, destfile, ...){
      tracker$called <<- tracker$called + 1
      file.create(destfile)
    },
    .package = "curl"
  )

  paths <- get_pedology_pdf(c("A", "A"), dirname = cache, verbose = FALSE)
  expect_length(paths, 1)
  expect_equal(tracker$called, 1)

})

test_that("get_pedology_pdf() handles download errors", {

  cache <- file.path(tempdir(), "pedology_pdf")
  dir.create(cache, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    curl_download = function(...) stop("download failed"),
    .package = "curl"
  )

  expect_message(
    get_pedology_pdf("A", dirname = cache, verbose = FALSE),
    "Failed to download"
  )

})

test_that("get_pedology_pdf() verbose prints message", {

  cache <- file.path(tempdir(), "pedology_pdf")
  dir.create(cache, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    curl_download = function(...) "filepath",
    .package = "curl"
  )

  expect_message(
    get_pedology_pdf("A", dirname = cache, verbose = TRUE),
    "UCS A saved"
  )

})

test_that("get_pedology_pdf() quiet is silent", {

  cache <- file.path(tempdir(), "pedology_pdf")
  dir.create(cache, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    curl_download = function(url, destfile, ...){
      file.create(destfile)
    },
    .package = "curl"
  )

  expect_no_message(
    get_pedology_pdf("A", dirname = cache, verbose = FALSE)
  )

})




test_that("download_guides() downloads, caches and overwrites", {

  cache <- tempfile("guides_")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- 0L

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet) {
      tracker$called <- tracker$called + 1L
      file.create(destfile)
    },
    .package = "curl"
  )

  # Download missing file
  path <- download_guides(cache = cache, verbose = FALSE)

  expect_equal(tracker$called, 1L)
  expect_true(file.exists(path))
  expect_equal(
    path,
    file.path(cache, "guides_stations_forestieres.gpkg")
  )

  # Reuse cached file
  download_guides(cache = cache, verbose = FALSE, overwrite = FALSE)
  expect_equal(tracker$called, 1L)

  # Overwrite existing file
  download_guides(cache = cache, verbose = FALSE, overwrite = TRUE)
  expect_equal(tracker$called, 2L)
})

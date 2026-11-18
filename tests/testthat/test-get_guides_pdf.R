fake_gpkg <- function() {

  guides <- Rsequoia2:::seq_poly |>
    transform(
      id = 1:3,
      nom = c("Guide A", "Guide B", "Guide C")
    )

  guides_urls <- data.frame(
    guide_id = c(1, 1, 2, 3),
    url = c(
      "https://example.org/A.pdf",
      "https://example.org/A-alt.pdf",
      "https://example.org/B.pdf",
      "https://example.org/C.pdf"
    )
  )

  stations <- data.frame(
    guide_id = rep(1:3, 3),
    nom = rep(sprintf("Station %s", LETTERS[1:3]), 3)
  )

  path <- tempfile(fileext = ".gpkg")

  sf::write_sf(guides, path, layer = "guides")
  sf::write_sf(guides_urls, path, layer = "guides_urls")
  sf::write_sf(stations, path, layer = "stations")

  path
}

test_that("get_guide_pdf() downloads, caches and overwrites", {

  gpkg <- fake_gpkg()
  dirname <- tempfile("guides_")
  on.exit(unlink(c(gpkg, dirname), recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$called <- 0L

  testthat::local_mocked_bindings(
    download_guides = function(...) gpkg,
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet, mode) {
      tracker$called <- tracker$called + 1L
      writeLines(paste("version", tracker$called), destfile)
    },
    .package = "curl"
  )

  # Download missing PDF
  out <- get_guide_pdf(1, dirname = dirname, verbose = FALSE)

  expect_equal(tracker$called, 1L)
  expect_true(file.exists(file.path(dirname, "guide_a.pdf")))
  expect_equal(out, normalizePath(dirname))

  # Reuse cached PDF
  get_guide_pdf(1, dirname = dirname, verbose = FALSE)
  expect_equal(tracker$called, 1L)

  # Overwrite existing PDF
  get_guide_pdf(1, dirname = dirname, verbose = FALSE, overwrite = TRUE)
  expect_equal(tracker$called, 2L)
})


test_that("get_guide_pdf() downloads multiple guides", {

  gpkg <- fake_gpkg()
  dirname <- tempfile("guides_")
  on.exit(unlink(c(gpkg, dirname), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_guides = function(...) gpkg,
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet, mode) {
      file.create(destfile)
    },
    .package = "curl"
  )

  get_guide_pdf(1:3, dirname = dirname, verbose = FALSE)

  expect_true(all(file.exists(
    file.path(dirname, c("guide_a.pdf", "guide_b.pdf", "guide_c.pdf"))
  )))
})


test_that("get_guide_pdf() tries alternative URLs", {

  gpkg <- fake_gpkg()
  dirname <- tempfile("guides_")
  on.exit(unlink(c(gpkg, dirname), recursive = TRUE), add = TRUE)

  tracker <- new.env(parent = emptyenv())
  tracker$urls <- character()

  testthat::local_mocked_bindings(
    download_guides = function(...) gpkg,
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    curl_download = function(url, destfile, quiet, mode) {
      tracker$urls <- c(tracker$urls, url)

      # First URL fails, second succeeds
      if (length(tracker$urls) == 1L) stop("Download failed")

      file.create(destfile)
    },
    .package = "curl"
  )

  get_guide_pdf(1, dirname = dirname, verbose = FALSE)

  expect_setequal(
    tracker$urls,
    c("https://example.org/A.pdf", "https://example.org/A-alt.pdf")
  )
  expect_length(tracker$urls, 2)
  expect_true(file.exists(file.path(dirname, "guide_a.pdf")))
})


test_that("get_guide_pdf() rejects invalid guide IDs", {

  gpkg <- fake_gpkg()
  dirname <- tempfile("guides_")
  on.exit(unlink(c(gpkg, dirname), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    download_guides = function(...) gpkg,
    .package = "Rsequoia2"
  )

  expect_error(
    get_guide_pdf(character(), dirname = dirname, verbose = FALSE),
    "valid IDs"
  )

  expect_error(
    get_guide_pdf(NA, dirname = dirname, verbose = FALSE),
    "valid IDs"
  )

  expect_error(
    get_guide_pdf(99, dirname = dirname, verbose = FALSE),
    "No download URLs found"
  )
})

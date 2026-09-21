test_that("download_lidar() aborts when x is not sf or sfc", {
  expect_error(
    download_lidar(data.frame(a = 1), "mnt"),
    "must be an .*sf.* or .*sfc.* object"
  )
})

test_that("download_lidar() rejects invalid key", {
  x <- Rsequoia2:::seq_poly
  expect_error(
    download_lidar(x, key = "invalid", verbose = FALSE),
    "'arg' should be one of"
  )
})

test_that("download_lidar() aborts when no LIDAR tile available", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) data.frame(),
    .package = "happign"
  )

  download_called <- FALSE
  testthat::local_mocked_bindings(
    seq_multi_download = function(...) {
      download_called <<- TRUE
      TRUE
    },
    .package = "Rsequoia2"
  )

  x <- Rsequoia2:::seq_poly
  expect_error(download_lidar(x, "mnt", verbose = FALSE), "No LIDAR MNT tile found")

  expect_false(download_called)
})

test_that("download_lidar() passes urls, destfiles and options to seq_multi_download()", {

  cache <- tempfile("lidar_")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  captured <- list()

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      captured$layer <<- layer

      data.frame(
        url_mnh = c(
          "https://example.com/wms?FILENAME=tile_1.tif",
          "https://example.com/wms?FILENAME=tile_2.tif"
        ),
        coordonnees_nw = c("0001-0001", "0002-0002")
      )
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    seq_multi_download = function(urls, destfiles, overwrite, verbose, max_tries){
      captured$urls <<- urls
      captured$destfiles <<- destfiles
      captured$overwrite <<- overwrite
      captured$verbose <<- verbose
      captured$max_tries <<- max_tries
      TRUE
    },
    .package = "Rsequoia2"
  )

  res <- download_lidar(
    x = Rsequoia2:::seq_poly,
    key = "mnh",
    cache = cache,
    overwrite = TRUE,
    verbose = FALSE,
    max_tries = 7
  )

  expected_files <- file.path(cache, c("tile_1.tif", "tile_2.tif"))

  expect_identical(
    captured$layer,
    "IGNF_LIDAR-HD_METADONNEE:metadata"
  )
  expect_identical(
    captured$urls,
    c(
      "https://example.com/wms?FILENAME=tile_1.tif",
      "https://example.com/wms?FILENAME=tile_2.tif"
    )
  )

  expect_identical(captured$destfiles, expected_files)
  expect_true(captured$overwrite)
  expect_false(captured$verbose)
  expect_identical(captured$max_tries, 7)

  expect_identical(res, expected_files)
})

test_that("download_lidar() creates cache directory when missing", {
  cache <- file.path(tempfile("lidar_"), "cache")
  on.exit(unlink(dirname(cache), recursive = TRUE, force = TRUE), add = TRUE)

  expect_false(dir.exists(cache))

  testthat::local_mocked_bindings(
    get_wfs = function(...) {
      data.frame(
        url_mnt = "https://example.com/wms?FILENAME=tile.tif",
        coordonnees_nw = "0001-0001"
      )
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    seq_multi_download = function(...) TRUE,
    .package = "Rsequoia2"
  )

  download_lidar(
    x = Rsequoia2:::seq_poly,
    key = "mnt",
    cache = cache,
    verbose = FALSE
  )

  expect_true(dir.exists(cache))
})

test_that("download_lidar() ignores metadata without the requested product", {
  captured <- list()

  testthat::local_mocked_bindings(
    get_wfs = function(...) {
      data.frame(
        url_mnt = c(
          NA_character_,
          "",
          "https://example.com/wms?FILENAME=available.tif"
        ),
        coordonnees_nw = c("0001-0001", "0002-0002", "0003-0003")
      )
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    seq_multi_download = function(urls, destfiles, ...) {
      captured$urls <<- urls
      captured$destfiles <<- destfiles
      TRUE
    },
    .package = "Rsequoia2"
  )

  cache <- tempfile("lidar_")
  on.exit(unlink(cache, recursive = TRUE, force = TRUE), add = TRUE)

  download_lidar(
    x = Rsequoia2:::seq_poly,
    key = "mnt",
    cache = cache,
    verbose = FALSE
  )

  expect_identical(
    captured$urls,
    "https://example.com/wms?FILENAME=available.tif"
  )
  expect_identical(captured$destfiles, file.path(cache, "available.tif"))
})

test_that("download_lidar() aborts when the requested product is unavailable", {
  testthat::local_mocked_bindings(
    get_wfs = function(...) {
      data.frame(
        url_mns = c(NA_character_, ""),
        coordonnees_nw = c("0001-0001", "0002-0002")
      )
    },
    .package = "happign"
  )

  download_called <- FALSE
  testthat::local_mocked_bindings(
    seq_multi_download = function(...) {
      download_called <<- TRUE
      TRUE
    },
    .package = "Rsequoia2"
  )

  expect_error(
    download_lidar(Rsequoia2:::seq_poly, "mns", verbose = FALSE),
    "No LIDAR MNS tile found"
  )
  expect_false(download_called)
})

test_that("download_lidar() aborts when a filename cannot be parsed", {
  testthat::local_mocked_bindings(
    get_wfs = function(...) {
      data.frame(
        url_mnt = "https://example.com/wms?REQUEST=GetMap",
        coordonnees_nw = "0001-0001"
      )
    },
    .package = "happign"
  )

  expect_error(
    download_lidar(Rsequoia2:::seq_poly, "mnt", verbose = FALSE),
    "Could not determine a GeoTIFF filename"
  )
})

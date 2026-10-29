test_that("get_guides retrieves and filters spatial data", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  make_polygon <- function(xmin, ymin, xmax, ymax) {
    sf::st_polygon(list(
      rbind(
        c(xmin, ymin),
        c(xmax, ymin),
        c(xmax, ymax),
        c(xmin, ymax),
        c(xmin, ymin)
      )
    ))
  }

  guides <- sf::st_sf(
    id = c(1L, 2L),
    geometry = sf::st_sfc(
      make_polygon(0, 0, 10, 10),
      make_polygon(1000, 1000, 1010, 1010),
      crs = 2154
    )
  )

  guides_urls <- sf::st_as_sf(
    data.frame(
      guide_id = c(1L, 2L),
      url = c("guide1.pdf", "guide2.pdf"),
      x = c(5, 1005),
      y = c(5, 1005)
    ),
    coords = c("x", "y"),
    crs = 2154
  )

  stations <- sf::st_as_sf(
    data.frame(
      guide_id = c(1L, 2L),
      station = c("Station 1", "Station 2"),
      x = c(5, 1005),
      y = c(5, 1005)
    ),
    coords = c("x", "y"),
    crs = 2154
  )

  x <- sf::st_sfc(
    make_polygon(0, 0, 20, 20),
    crs = 2154
  )

  testthat::local_mocked_bindings(
    download_guides = function(...) file.path(tmp, "gs.gpkg"),
    seq_envelope = function(x, buffer) x
  )

  testthat::local_mocked_bindings(
    read_sf = function(dsn, layer, ...) {
      switch(
        layer,
        guides = guides,
        guides_urls = guides_urls,
        stations = stations
      )
    },
    .package = "sf"
  )

  result <- get_guides(x, cache = tmp, verbose = FALSE)

  expect_named(result, c("guides", "guides_urls", "stations"))
  expect_s3_class(result$guides, "sf")
  expect_s3_class(result$guides_urls, "sf")
  expect_s3_class(result$stations, "sf")

  expect_equal(result$guides$id, 1L)
  expect_equal(result$guides_urls$guide_id, 1L)
  expect_equal(result$stations$guide_id, 1L)
})


test_that("get_guides returns NULL when no guides intersect", {

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  guides <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_point(c(1000, 1000)),
      crs = 2154
    )
  )

  guides_urls <- sf::st_sf(
    guide_id = integer(),
    url = character(),
    geometry = sf::st_sfc(crs = 2154)
  )

  stations <- sf::st_sf(
    guide_id = integer(),
    geometry = sf::st_sfc(crs = 2154)
  )

  x <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)

  testthat::local_mocked_bindings(
    download_guides = function(...) file.path(tmp, "gs.gpkg"),
    seq_envelope = function(x, buffer) x
  )

  testthat::local_mocked_bindings(
    read_sf = function(dsn, layer, ...) {
      switch(
        layer,
        guides = guides,
        guides_urls = guides_urls,
        stations = stations
      )
    },
    .package = "sf"
  )

  expect_null(
    get_guides(x, cache = tmp, verbose = FALSE)
  )
})


test_that("get_guides validates x", {

  expect_error(
    get_guides(x = data.frame(a = 1))
  )

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  expect_error(
    get_guides(x = sf::st_sfc(crs = 2154))
  )
})


test_that("get_guides validates cache and logical arguments", {

  x <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    crs = 2154
  )

  expect_error(
    get_guides(x, cache = "")
  )

  expect_error(
    get_guides(x, overwrite = 1)
  )

  expect_error(
    get_guides(x, verbose = 1)
  )
})

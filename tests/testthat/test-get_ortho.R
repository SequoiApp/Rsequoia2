
# Helpers -----------------------------------------------------------------

make_test_ortho <- function() {
  r <- terra::rast(
    nrows = 100, ncols = 100, nlyrs = 4,
    xmin = 0, xmax = 100, ymin = 0, ymax = 100,
    crs = "EPSG:2154"
  )

  terra::values(r) <- cbind(
    rep(255, terra::ncell(r)),
    rep(128, terra::ncell(r)),
    rep(64, terra::ncell(r)),
    rep(0, terra::ncell(r))
  )

  r
}

make_test_points <- function(coords) {
  sf::st_sfc(
    lapply(seq_len(nrow(coords)), function(i) sf::st_point(coords[i, ])),
    crs = 2154
  )
}

# Validation --------------------------------------------------------------

test_that("get_ortho() validates x", {
  expect_error(get_ortho(42), "should be of class")
  expect_error(get_ortho("a"), "should be of class")
})

test_that("get_ortho() validates type", {
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  expect_error(get_ortho(x, type = "foo"), "type")
  expect_error(get_ortho(x, type = c("irc", "rgb")), "type")
})

test_that("get_ortho() validates buffer", {
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  for (b in list(-1, NA_real_, Inf, NaN, "10", c(1, 2))) {
    expect_error(get_ortho(x, type="irc", buffer = b), "finite, non-negative")
  }
})

test_that("get_ortho() aborts on empty geometry", {
  x <- sf::st_sfc(sf::st_geometrycollection(), crs = 2154)

  expect_error(
    get_ortho(x, type="irc", verbose = FALSE),
    "has no valid geometry after cleaning"
  )
})

test_that("get_ortho() requires positive buffer for points and lines", {
  point <- make_test_points(matrix(c(50, 50), ncol = 2))
  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(20, 20, 80, 80), ncol = 2, byrow = TRUE)),
    crs = 2154
  )

  expect_error(get_ortho(point, type="irc", buffer = 0), "must be positive")
  expect_error(get_ortho(line, type="irc", buffer = 0), "must be positive")
})

# Raster output -----------------------------------------------------------

test_that("get_ortho() returns a valid RGB SpatRaster", {
  skip_on_os("mac")

  r_fake <- make_test_ortho()
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  for (type in c("irc", "rgb")) {
    ortho <- get_ortho(x, type = type, buffer = 15, verbose = FALSE)

  expect_s4_class(ortho, "SpatRaster")
    expect_equal(terra::nlyr(ortho), 4)
    expect_true(terra::has.RGB(ortho))
    expect_equal(names(ortho), c("red", "green", "blue", "alpha"))
  }
})

test_that("get_ortho() masks outside buffer area", {
  skip_on_os("mac")

  r_fake <- make_test_ortho()
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  ortho <- get_ortho(x, type="irc", buffer = 15, verbose = FALSE)
  vals <- terra::values(ortho)

  expect_true(any(is.na(vals)))
  expect_true(any(vals == 128, na.rm = TRUE))
})

# Geometry processing -----------------------------------------------------

test_that("get_ortho() dissolves overlapping buffers", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(40, 50, 45, 50),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_ortho()
  captured <- list()

  local_mocked_bindings(
    get_wmts = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_ortho(x, type="irc", buffer = 10, verbose = FALSE)

  expect_length(captured, 1)
  expect_s3_class(captured[[1]], "sfc")
  expect_equal(as.character(sf::st_geometry_type(captured[[1]])), "POLYGON")

  expected <- x |>
    sf::st_buffer(10) |>
    sf::st_union()

  expect_true(sf::st_equals(captured[[1]], expected, sparse = FALSE)[1, 1])
})

test_that("get_ortho() downloads each disjoint polygon separately", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(20, 20, 80, 80),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_ortho()
  captured <- list()

  local_mocked_bindings(
    get_wmts = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_ortho(x, type="irc", buffer = 10, verbose = FALSE)

  expect_length(captured, 2)
  expect_true(all(vapply(captured, length, integer(1)) == 1L))
  expect_true(all(vapply(captured, function(g) {
    as.character(sf::st_geometry_type(g)) == "POLYGON"
  }, logical(1))))

  expect_false(sf::st_intersects(captured[[1]], captured[[2]], sparse = FALSE)[1, 1])
})

test_that("get_ortho() merges overlapping and keeps disconnected areas", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(20, 20, 25, 20, 80, 80),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_ortho()
  captured <- list()

  local_mocked_bindings(
    get_wmts = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_ortho(x, type="irc", buffer = 10, verbose = FALSE)

  # Three input points, but only two disconnected buffered areas
  expect_length(captured, 2)

  actual <- do.call(c, captured) |> sf::st_union()
  expected <- x |> sf::st_buffer(10) |> sf::st_union()

  expect_true(sf::st_equals(actual, expected, sparse = FALSE)[1, 1])
})

test_that("get_ortho() supports sf and sfc inputs", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_ortho()

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  for (input in list(x, sf::st_sf(id = 1, geometry = x))) {
    ortho <- get_ortho(input, type="irc", buffer = 10, verbose = FALSE)
    expect_s4_class(ortho, "SpatRaster")
  }
})

test_that("get_ortho() supports lines and polygons", {
  skip_on_os("mac")

  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(30, 30, 70, 70), ncol = 2, byrow = TRUE)),
    crs = 2154
  )
  poly <- sf::st_buffer(make_test_points(matrix(c(50, 50), ncol = 2)), 10)

  r_fake <- make_test_ortho()

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  expect_s4_class(get_ortho(line, type="irc", buffer = 5, verbose = FALSE), "SpatRaster")
  expect_s4_class(get_ortho(poly, type="irc", buffer = 0, verbose = FALSE), "SpatRaster")
})

# Temporary files ---------------------------------------------------------

test_that("get_ortho() writes temporary raster files", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_ortho()
  files <- character()

  local_mocked_bindings(
    get_wmts = function(..., filename) {
      files <<- c(files, filename)
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_ortho(x, type="irc", buffer = 10, verbose = FALSE)

  expect_length(files, 1)
  expect_true(all(file.exists(files)))
  expect_match(basename(files[1]), "^r_\\d{3}\\.tif$")
})

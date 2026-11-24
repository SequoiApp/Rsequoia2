
# Helpers -----------------------------------------------------------------

make_test_rge <- function(value = 10) {
  r <- terra::rast(
    nrows = 100, ncols = 100,
    xmin = 0, xmax = 100,
    ymin = 0, ymax = 100,
    crs = "EPSG:2154"
  )
  terra::values(r) <- value
  r
}

make_test_points <- function(coords) {
  sf::st_sfc(
    lapply(seq_len(nrow(coords)), function(i) sf::st_point(coords[i, ])),
    crs = 2154
  )
}

# Validation --------------------------------------------------------------

test_that("get_rge() validates x", {
  expect_error(get_rge(42), "must be .*sf.* or .*sfc")
  expect_error(get_rge("a"), "must be .*sf.* or .*sfc")
})

test_that("get_rge() validates key", {
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  expect_error(get_rge(x, key = "invalid"), "arg")
})

test_that("get_rge() validates buffer", {
  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  for (b in list(-1, NA_real_, Inf, NaN, "10", c(1, 2))) {
    expect_error(get_rge(x, buffer = b), "finite")
  }
})

test_that("get_rge() aborts on empty geometry", {
  x <- sf::st_sfc(sf::st_geometrycollection(), crs = 2154)

  expect_error(
    get_rge(x, verbose = FALSE),
    "has no valid geometry after cleaning"
  )
})

test_that("get_rge() requires positive buffer for points and lines", {
  point <- make_test_points(matrix(c(50, 50), ncol = 2))
  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(20, 20, 80, 80), ncol = 2, byrow = TRUE)),
    crs = 2154
  )

  expect_error(get_rge(point, buffer = 0), "must be positive")
  expect_error(get_rge(line, buffer = 0), "must be positive")
})

# Raster output -----------------------------------------------------------

test_that("get_rge() returns a named SpatRaster for MNT and MNS", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_rge()

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  for (key in c("mnt", "mns")) {
    r <- get_rge(x, key = key, buffer = 10, verbose = FALSE)

    expect_s4_class(r, "SpatRaster")
    expect_identical(names(r), paste0(key, "_rge"))
    expect_true(terra::same.crs(r, "EPSG:2154"))
  }
})

test_that("get_rge() uses correct WMS layer", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_rge()
  layers <- character()

  local_mocked_bindings(
    get_wms_raster = function(..., layer, filename) {
      layers <<- c(layers, layer)
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_rge(x, key = "mnt", buffer = 10, verbose = FALSE)
  get_rge(x, key = "mns", buffer = 10, verbose = FALSE)

  expect_identical(layers, c(
    "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
    "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES.MNS"
  ))
})

test_that("get_rge() masks outside buffer area", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_rge(value = 25)

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  r <- get_rge(x, buffer = 10, verbose = FALSE)
  vals <- terra::values(r)

  expect_true(any(is.na(vals)))
  expect_true(any(vals == 25, na.rm = TRUE))
})

# Geometry processing -----------------------------------------------------

test_that("get_rge() dissolves overlapping buffers", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(40, 50, 45, 50),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_rge()
  captured <- list()

  local_mocked_bindings(
    get_wms_raster = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_rge(x, buffer = 10, verbose = FALSE)

  expect_length(captured, 1)
  expect_s3_class(captured[[1]], "sfc")
  expect_equal(as.character(sf::st_geometry_type(captured[[1]])), "POLYGON")

  expected <- x |>
    sf::st_buffer(10) |>
    sf::st_union()

  expect_true(sf::st_equals(captured[[1]], expected, sparse = FALSE)[1, 1])
})

test_that("get_rge() downloads each disjoint polygon separately", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(20, 20, 80, 80),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_rge()
  captured <- list()

  local_mocked_bindings(
    get_wms_raster = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_rge(x, buffer = 10, verbose = FALSE)

  expect_length(captured, 2)
  expect_true(all(vapply(captured, length, integer(1)) == 1L))
  expect_true(all(vapply(captured, function(g) {
    as.character(sf::st_geometry_type(g)) == "POLYGON"
  }, logical(1))))

  expect_false(
    sf::st_intersects(captured[[1]], captured[[2]], sparse = FALSE)[1, 1]
  )
})

test_that("get_rge() merges overlapping and preserves disjoint areas", {
  skip_on_os("mac")

  x <- make_test_points(matrix(
    c(20, 20, 25, 20, 80, 80),
    ncol = 2, byrow = TRUE
  ))

  r_fake <- make_test_rge()
  captured <- list()

  local_mocked_bindings(
    get_wms_raster = function(x, ..., filename) {
      captured[[length(captured) + 1L]] <<- x
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_rge(x, buffer = 10, verbose = FALSE)

  expect_length(captured, 2)

  actual <- do.call(c, captured) |> sf::st_union()
  expected <- x |> sf::st_buffer(10) |> sf::st_union()

  expect_true(sf::st_equals(actual, expected, sparse = FALSE)[1, 1])
})

test_that("get_rge() supports sf and sfc inputs", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_rge()

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  for (input in list(x, sf::st_sf(id = 1, geometry = x))) {
    r <- get_rge(input, buffer = 10, verbose = FALSE)
    expect_s4_class(r, "SpatRaster")
  }
})

test_that("get_rge() supports lines and polygons", {
  skip_on_os("mac")

  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(30, 30, 70, 70), ncol = 2, byrow = TRUE)),
    crs = 2154
  )
  poly <- sf::st_buffer(make_test_points(matrix(c(50, 50), ncol = 2)), 10)
  r_fake <- make_test_rge()

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  expect_s4_class(get_rge(line, buffer = 5, verbose = FALSE), "SpatRaster")
  expect_s4_class(get_rge(poly, buffer = 0, verbose = FALSE), "SpatRaster")
})

# MNH ---------------------------------------------------------------------

test_that("get_rge() computes MNH from MNS minus MNT", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))

  local_mocked_bindings(
    get_wms_raster = function(..., layer, filename) {
      value <- if (grepl("\\.MNS$", layer)) 30 else 20
      terra::writeRaster(make_test_rge(value), filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  r <- get_rge(x, key = "mnh", buffer = 10, verbose = FALSE)

  expect_s4_class(r, "SpatRaster")
  expect_identical(names(r), "mnh_rge")

  vals <- terra::values(r)
  expect_true(any(!is.na(vals)))
  expect_equal(unique(na.omit(as.vector(vals))), 10)
})

# Temporary files ---------------------------------------------------------

test_that("get_rge() writes temporary raster files", {
  skip_on_os("mac")

  x <- make_test_points(matrix(c(50, 50), ncol = 2))
  r_fake <- make_test_rge()
  files <- character()

  local_mocked_bindings(
    get_wms_raster = function(..., filename) {
      files <<- c(files, filename)
      terra::writeRaster(r_fake, filename, overwrite = TRUE)
    },
    .package = "happign"
  )

  get_rge(x, key = "mnt", buffer = 10, verbose = FALSE)

  expect_length(files, 1)
  expect_true(all(file.exists(files)))
  expect_match(basename(files[1]), "^rge_mnt_.*\\.tif$")
})


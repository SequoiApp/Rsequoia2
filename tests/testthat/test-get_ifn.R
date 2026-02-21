test_that("get_ifn() returns sf with expected structure", {

  # point
  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  # polygone
  poly <- sf::st_sf(
    codeser = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-1, -1,
          1, -1,
          1,  1,
          -1,  1,
          -1, -1),
        ncol = 2,
        byrow = TRUE
      ))),
      crs = 2154
    )
  )

  shp <- "fake/path/ser_l93.shp"

  testthat::local_mocked_bindings(
    list.files = function(...) shp,
    .package = "base"
  )

  testthat::local_mocked_bindings(
    st_read = function(...) poly,
    .package = "sf"
  )

  ifn <- get_ifn(point, "ser")

  expect_s3_class(ifn, "sf")
  expect_gt(nrow(ifn), 0)
})

test_that("get_ifn() fails with invalid key", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  expect_error(
    get_ifn(point, key = "invalid"),
    "must be one of"
  )
})

test_that("get_ifn() returns NULL when no spatial intersection exists", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  poly <- sf::st_sf(
    codeser = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(10,10, 11,10, 11,11, 10,11, 10,10),
        ncol = 2, byrow = TRUE
      ))),
      crs = 2154
    )
  )

  shp <- "fake/path/ser_l93.shp"

  testthat::local_mocked_bindings(
    list.files = function(...) shp,
    .package = "base"
  )

  testthat::local_mocked_bindings(
    st_read = function(...) poly,
    st_intersects = function(x, y, sparse = FALSE) {
      matrix(FALSE, nrow = nrow(x), ncol = 1)
    },
    .package = "sf"
  )

  res <- get_ifn(point, "ser")

  expect_null(res)
})

test_that("get_ifn() assigns CRS when missing on IFN layer", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  poly <- sf::st_sf(
    codeser = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0,0, 1,0, 1,1, 0,1, 0,0),
        ncol = 2, byrow = TRUE
      )))
    )
  )
  sf::st_crs(poly) <- NA

  shp <- "fake/path/ser_l93.shp"

  testthat::local_mocked_bindings(
    list.files = function(...) shp,
    .package = "base"
  )

  testthat::local_mocked_bindings(
    st_read = function(...) poly,
    st_intersects = function(x, y, sparse = FALSE) matrix(TRUE, nrow = 1, ncol = 1),
    .package = "sf"
  )

  res <- get_ifn(point, "ser")

  expect_equal(sf::st_crs(res), sf::st_crs(point))
})

test_that("get_ifn() fails when key has length > 1", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  expect_error(
    get_ifn(point, c("ser", "rfn")),
    "must be one of"
  )
})

test_that("get_ifn() fails when no shapefile is found", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  testthat::local_mocked_bindings(
    list.files = function(...) character(0),
    .package = "base"
  )

  expect_error(
    get_ifn(point, "ser"),
    "No shapefile found"
  )
})

test_that("get_ifn() keeps CRS when identical to input", {

  point <- sf::st_as_sf(
    sf::st_sfc(sf::st_point(c(700000, 6600000)), crs = 2154)
  )

  poly <- sf::st_sf(
    codeser = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(699000, 6599000),
        c(701000, 6599000),
        c(701000, 6601000),
        c(699000, 6601000),
        c(699000, 6599000)
      ))),
      crs = 2154
    )
  )

  testthat::local_mocked_bindings(
    list.files = function(...) "ser_l93.shp",
    .package = "base"
  )

  testthat::local_mocked_bindings(
    st_read = function(...) poly,
    .package = "sf"
  )

  ifn <- get_ifn(point, key = "ser")

  expect_equal(sf::st_crs(ifn), sf::st_crs(point))
})


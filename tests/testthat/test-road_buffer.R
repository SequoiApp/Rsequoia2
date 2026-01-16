test_that("road_buffer() returns NULL when no line matches the requested type", {

  type_field <- seq_field("type")$name
  name_field <- seq_field("name")$name

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 5, 5), ncol = 2, byrow = TRUE)),
      crs = 2154
    )
  )

  x[[type_field]] <- "RC"
  x[[name_field]] <- "Road A"

  out <- road_buffer(x, type = "RN", dist = 5)

  expect_null(out)
})

test_that("road_buffer() returns a polygon sf for a valid road type", {

  type_field <- seq_field("type")$name
  name_field <- seq_field("name")$name

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 0, 10), ncol = 2, byrow = TRUE)),
      crs = 2154
    )
  )

  x[[type_field]] <- "RN"
  x[[name_field]] <- "Main road"

  out <- road_buffer(x, type = "RN", dist = 5)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_true(type_field %in% names(out))
  expect_equal(out[[type_field]], "RN")
  expect_true(inherits(sf::st_geometry(out), "sfc_POLYGON"))
})

test_that("road_buffer() aggregates lines by type", {

  type_field <- seq_field("type")$name

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 0, 5), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 5, 0, 10), ncol = 2, byrow = TRUE)),
      crs = 2154
    )
  )

  x[[type_field]] <- c("RN", "RN")

  out <- road_buffer(x, type = "RN", dist = 3)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(out[[type_field]], "RN")
})

test_that("road_buffer() aggregates multiple lines of the same type into one polygon", {

  type_field <- seq_field("type")$name

  x <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 0, 5), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 5, 0, 10), ncol = 2, byrow = TRUE)),
      crs = 2154
    )
  )

  x[[type_field]] <- c("RN", "RN")

  out <- road_buffer(x, type = "RN", dist = 3)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(out[[type_field]], "RN")
  expect_true(inherits(sf::st_geometry(out), "sfc_POLYGON"))
})

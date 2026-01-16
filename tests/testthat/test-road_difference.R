test_that("road_difference() returns NULL if buffers list is empty", {
  out <- road_difference(list(), order = c("RN", "RD"))
  expect_null(out)
})

test_that("road_difference() skips missing types in order", {
  type_field <- seq_field("type")$name

  # Only RC buffer exists
  rc <- Rsequoia2:::seq_poly[1,]
  rc[[type_field]] <- "RC"

  buffers <- list(RC = rc)

  out <- road_difference(buffers, order = c("RN", "RC"))
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(out[[type_field]], "RC")
})

test_that("road_difference() performs hierarchical difference correctly", {
  type_field <- seq_field("type")$name

  rn <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 0,4, 4,4, 4,0, 0,0), ncol=2, byrow=TRUE)))
    ),
    crs = 2154
  )
  rn[[type_field]] <- "RN"

  rd <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(2,2, 2,5, 5,5, 5,2, 2,2), ncol=2, byrow=TRUE)))
    ),
    crs = 2154
  )
  rd[[type_field]] <- "RD"

  buffers <- list(RN = rn, RD = rd)

  out <- road_difference(buffers, order = c("RN", "RD")) |> quiet()

  expect_s3_class(out, "sf")
  expect_true(setequal(unique(out[[type_field]]), c("RN", "RD")))

  rd_geom <- out[out[[type_field]]=="RD", ]
  rn_geom <- out[out[[type_field]]=="RN", ]
  if (nrow(rd_geom) > 0 && nrow(rn_geom) > 0) {
    expect_false(any(sf::st_overlaps(rd_geom, rn_geom)[[1]]))
  }
})

test_that("road_difference() drops empty geometries", {
  type_field <- seq_field("type")$name

  empty <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_geometrycollection()),
    crs = 2154
  )
  empty[[type_field]] <- "RN"

  buffers <- list(RN = empty)

  out <- road_difference(buffers, order = c("RN"))
  expect_null(out)
})

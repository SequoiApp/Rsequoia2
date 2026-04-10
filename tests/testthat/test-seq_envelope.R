test_that("seq_envelope() works with simple points", {
  pts <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0,0)),
      sf::st_point(c(1,1)),
      sf::st_point(c(2,0)),
      crs = 4326
    )
  )

  result <- seq_envelope(pts, buffer = 0.1)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is(result, "POLYGON")))
  expect_equal(nrow(result), 3)   # chaque point -> 1 polygon
  expect_true(all(sf::st_is_valid(result)))
})

test_that("seq_envelope() works with polygons", {

  poly <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
      ))),
      crs = 2154
    )
  )

  result <- seq_envelope(poly, buffer = 20, crs = 4326)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
  expect_equal(sf::st_crs(result)$epsg, 4326)
})

test_that("seq_envelope() handles multipolygons", {

  mp <- sf::st_multipolygon(list(
    list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
    list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2)))
  ))

  sf_mp <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(mp, crs = 3857)
  )

  result <- seq_envelope(sf_mp, buffer = 100)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is(result, "POLYGON")))
  expect_true(all(sf::st_is_valid(result)))
})

test_that("seq_envelope() supports zero buffer distance", {


  pts <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(1,1)), crs = 4326)
  )

  result <- seq_envelope(pts, buffer = 0)

  # buffer(0) peut produire un GEOMETRYCOLLECTION -> on teste juste validite
  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
})

test_that("seq_envelope() fails gracefully on wrong inputs", {


  expect_error(seq_envelope(123, buffer = 10))
  expect_error(seq_envelope(data.frame(a = 1), buffer = 10))
  expect_error(seq_envelope(sf::st_sf(geometry = NA), buffer = 10))
})

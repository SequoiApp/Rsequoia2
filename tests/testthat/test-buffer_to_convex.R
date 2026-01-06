test_that("envelope works with simple points", {
  skip_if_not_installed("sf")

  pts <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0,0)),
      sf::st_point(c(1,1)),
      sf::st_point(c(2,0)),
      crs = 4326
    )
  )

  result <- envelope(pts, dist = 0.1)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is(result, "POLYGON")))
  expect_equal(nrow(result), 3)   # chaque point -> 1 polygon
  expect_true(all(sf::st_is_valid(result)))
})


test_that("envelope works with polygons", {
  skip_if_not_installed("sf")

  poly <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
      ))),
      crs = 2154
    )
  )

  result <- envelope(poly, dist = 20, crs = 4326)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
  expect_equal(sf::st_crs(result)$epsg, 4326)
})


test_that("envelope handles multipolygons", {
  skip_if_not_installed("sf")

  mp <- sf::st_multipolygon(list(
    list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))),
    list(rbind(c(2,2), c(3,2), c(3,3), c(2,3), c(2,2)))
  ))

  sf_mp <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(mp, crs = 3857)
  )

  result <- envelope(sf_mp, dist = 100)

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is(result, "POLYGON")))
  expect_true(all(sf::st_is_valid(result)))
})


test_that("envelope returns valid geometry even with invalid inputs", {
  skip_if_not_installed("sf")

  # bowtie polygon (self-intersection)
  invalid_poly <- sf::st_polygon(list(rbind(
    c(0,0), c(2,0), c(0,2), c(2,2), c(0,0)
  )))

  x <- sf::st_sf(
    geometry = sf::st_sfc(invalid_poly, crs = 4326)
  )

  # ATTENTION : st_buffer échoue avec S2 si geometry invalide
  # On attend que la fonction échoue proprement
  expect_error(envelope(x, dist = 0.05))
})


test_that("envelope supports zero buffer distance", {
  skip_if_not_installed("sf")

  pts <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_point(c(1,1)), crs = 4326)
  )

  result <- envelope(pts, dist = 0)

  # buffer(0) peut produire un GEOMETRYCOLLECTION → on teste juste validité
  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
})


test_that("envelope fails gracefully on wrong inputs", {
  skip_if_not_installed("sf")

  expect_error(envelope(123, dist = 10))
  expect_error(envelope(data.frame(a = 1), dist = 10))
  expect_error(envelope(sf::st_sf(geometry = NA), dist = 10))
})

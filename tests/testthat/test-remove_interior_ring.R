test_that("remove_interior_ring removes holes", {

  # --- Polygon with hole ----
  wkt <- "POLYGON((
    0 0, 0 10, 10 10, 10 0, 0 0
  ),(
    2 2, 2 4, 4 4, 4 2, 2 2
  ))"

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_as_sfc(wkt, crs = 2154)
  )

  res <- remove_interior_ring(x)

  # --- Checks ----
  geom <- sf::st_geometry(res)[[1]]

  testthat::expect_equal(length(geom), 1)  # plus de trous
})

test_that("remove_interior_ring keeps simple polygons unchanged", {

  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_as_sfc(wkt, crs = 2154)
  )

  res <- remove_interior_ring(x)

  testthat::expect_equal(
    sf::st_as_text(sf::st_geometry(x)),
    sf::st_as_text(sf::st_geometry(res))
  )
})

test_that("remove_interior_ring handles multipolygons", {

  wkt <- "MULTIPOLYGON(
    ((0 0,0 10,10 10,10 0,0 0),(2 2,2 4,4 4,4 2,2 2)),
    ((20 20,20 30,30 30,30 20,20 20))
  )"

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_as_sfc(wkt, crs = 2154)
  )

  res <- remove_interior_ring(x)

  # après cast → 2 polygons sans trous
  testthat::expect_equal(nrow(res), 2)

  geoms <- sf::st_geometry(res)
  testthat::expect_true(all(vapply(geoms, length, integer(1)) == 1))
})

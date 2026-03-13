test_that("clean_topology() fails on non-sf input", {
  expect_error(
    clean_topology(data.frame(x = 1)),
    "`x` must be an sf object"
  )
})

test_that("clean_topology removes overlaps", {

  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("qgisprocess")

  poly1 <- sf::st_polygon(list(rbind(
    c(0,0), c(2,0), c(2,2), c(0,2), c(0,0)
  )))

  poly2 <- sf::st_polygon(list(rbind(
    c(1.9,0), c(4,0), c(4,2), c(1.9,2), c(1.9,0)
  )))

  x <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(poly1, poly2, crs = 2154)
  )

  result <- clean_topology(x, snap_tolerance = 1)

  overlaps <- sf::st_overlaps(result, sparse = FALSE)

  expect_false(any(overlaps[upper.tri(overlaps)]))
})

test_that("clean_topology preserves holes", {

  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("qgisprocess")

  outer <- rbind(
    c(0,0), c(4,0), c(4,4), c(0,4), c(0,0)
  )

  hole <- rbind(
    c(1,1), c(3,1), c(3,3), c(1,3), c(1,1)
  )

  poly <- sf::st_polygon(list(outer, hole))

  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(poly, crs = 2154)
  )

  result <- clean_topology(x)

  expect_equal(
    lengths(sf::st_geometry(result)),
    lengths(sf::st_geometry(x))
  )
})

test_that("clean_topology preserves attributes", {

  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("qgisprocess")

  poly <- sf::st_polygon(list(rbind(
    c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
  )))

  x <- sf::st_sf(
    id = 1,
    value = "test",
    geometry = sf::st_sfc(poly, crs = 2154)
  )

  result <- clean_topology(x)

  expect_all_true(names(sf::st_drop_geometry(x)) %in% names(sf::st_drop_geometry(result)))
})

test_that("clean_topology removes small polygons", {

  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("qgisprocess")

  big <- sf::st_polygon(list(rbind(
    c(0,0), c(4,0), c(4,4), c(0,4), c(0,0)
  )))

  small <- sf::st_polygon(list(rbind(
    c(4,0), c(4.01,0), c(4.01,0.01), c(4,0.01), c(4,0)
  )))

  x <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(big, small, crs = 2154)
  )

  result <- clean_topology(x, min_area = 0.001)

  expect_lt(nrow(result), nrow(x))
})

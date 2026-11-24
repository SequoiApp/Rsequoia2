test_that("remove_small_geometries filters small polygons", {
  small <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), 5, 2, byrow = TRUE)))
  large <- sf::st_polygon(list(matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), 5, 2, byrow = TRUE)))
  x <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(small, large, crs = 2154))

  res <- remove_small_geometries(x, 500)

  expect_equal(nrow(res), 1)
  expect_equal(res$id, 2)
})

test_that("remove_small_geometries can return empty", {
  x <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), 5, 2, byrow = TRUE))),
      crs = 2154
    )
  )

  expect_equal(nrow(remove_small_geometries(x, 1000)), 0)
})

test_that("remove_small_geometries validates inputs", {
  expect_error(remove_small_geometries(1:10, 100))
  expect_error(remove_small_geometries(sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154), c(1, 2)))
})

test_that("remove_small_geometries with tol = 0 keeps all geometries", {
  x <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), 5, 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), 5, 2, byrow = TRUE))),
      crs = 2154
    )
  )

  res <- remove_small_geometries(x, 0)

  expect_equal(nrow(res), 2)
  expect_equal(res$id, 1:2)
})

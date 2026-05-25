test_that("remove_small_geometries filters small polygons", {

  # --- Data ----
  g <- sf::st_as_sfc(c(
    "POLYGON((0 0, 0 1, 1 1, 1 0, 0 0))",
    "POLYGON((0 0, 0 0.0001, 0.0001 0.0001, 0.0001 0, 0 0))"
  ), crs = 4326)

  x <- sf::st_sf(id = 1:2, geometry = g)

  # --- Run ----
  res <- remove_small_geometries(x, tol = 500)

  # --- Checks ----
  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$id, 1)
})

test_that("remove_small_geometries can return empty", {

  g <- sf::st_as_sfc(
    "POLYGON((0 0, 0 0.0001, 0.0001 0.0001, 0.0001 0, 0 0))",
    crs = 4326
  )

  x <- st_sf(id = 1, geometry = g)

  res <- remove_small_geometries(x, tol = 1000)

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("remove_small_geometries fails on bad inputs", {

  expect_error(remove_small_geometries(1:10, tol = 100))

  g <- sf::st_as_sfc("POINT(0 0)", crs = 4326)

  expect_error(remove_small_geometries(g, tol = c(1, 2)))
})

test_that("remove_small_geometries with tol = 0 keeps all geometries", {

  small_poly <- sf::st_polygon(list(matrix(c(
    0, 0,
    0.1, 0,
    0.1, 0.1,
    0, 0.1,
    0, 0
  ), ncol = 2, byrow = TRUE)))

  large_poly <- sf::st_polygon(list(matrix(c(
    0, 0,
    10, 0,
    10, 10,
    0, 10,
    0, 0
  ), ncol = 2, byrow = TRUE)))

  sf_obj <- sf::st_sf(
    id = c(1, 2),
    geometry = sf::st_sfc(small_poly, large_poly, crs = 2154)
  )

  result <- remove_small_geometries(sf_obj, tol = 0)

  expect_equal(nrow(result), 2)
  expect_true(all(result$id %in% c(1, 2)))
  expect_equal(sf::st_area(result), sf::st_area(sf_obj))
})

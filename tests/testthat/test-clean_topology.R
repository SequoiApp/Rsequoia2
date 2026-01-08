test_that("clean_topology() fails on non-sf input", {
  expect_error(
    clean_topology(data.frame(x = 1)),
    "`sf_obj` must be an sf object"
  )
})

test_that("clean_topology() runs GRASS v.clean on simple geometry", {

  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("qgisprocess")

  # Simple overlapping squares
  poly1 <- sf::st_polygon(list(rbind(
    c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)
  )))

  poly2 <- sf::st_polygon(list(rbind(
    c(1.9, 0), c(4, 0), c(4, 2), c(1.9, 2), c(1.9, 0)
  )))

  sf_obj <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(poly1, poly2, crs = 2154)
  )

  result <- clean_topology(
    sf_obj,
    tool = "snap",
    snap_tolerance = 0.2,
    min_area = 0.01,
    verbose = FALSE
  )

  expect_s3_class(result, "sf")
  expect_true(all(sf::st_is_valid(result)))
  expect_equal(nrow(result), nrow(sf_obj))
})

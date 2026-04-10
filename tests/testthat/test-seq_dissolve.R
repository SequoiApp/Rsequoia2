test_that("seq_dissolve() returns an sf object", {

  x <- Rsequoia2:::seq_poly

  res <- seq_dissolve(x)

  expect_s3_class(res, "sf")
})

test_that("seq_dissolve() fails with non-sf input", {

  expect_error(
    seq_dissolve(data.frame(a = 1)),
    class = "rlang_error"
  )
})

test_that("seq_dissolve() fails with non-polygon geometries", {

  x <- Rsequoia2:::seq_point

  expect_error(
    seq_dissolve(x),
    class = "rlang_error"
  )
})

test_that("seq_dissolve() dissolves touching polygons", {

  g1 <- sf::st_polygon(list(rbind(
    c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
  )))

  g2 <- sf::st_polygon(list(rbind(
    c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)
  )))

  x <- sf::st_sf(geom = sf::st_sfc(g1, g2, crs = 2154))

  res <- seq_dissolve(x, buffer = 0)

  expect_equal(nrow(res), 1)
})

test_that("seq_dissolve() merges close polygons with buffer", {

  g1 <- sf::st_polygon(list(rbind(
    c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
  )))

  g2 <- sf::st_polygon(list(rbind(
    c(1.05,0), c(2.05,0), c(2.05,1), c(1.05,1), c(1.05,0)
  )))

  x <- sf::st_sf(geom = sf::st_sfc(g1, g2, crs = 2154))

  res_no_buffer <- seq_dissolve(x, buffer = 0)
  res_buffer <- seq_dissolve(x, buffer = 0.1)

  expect_equal(nrow(res_no_buffer), 2)
  expect_equal(nrow(res_buffer), 1)
})

test_that("seq_dissolve() returns valid geometries", {

  x <- Rsequoia2:::seq_poly

  res <- seq_dissolve(x)

  expect_true(all(sf::st_is_valid(res)))
})

test_that("seq_dissolve() preserves area approximately", {

  x <- Rsequoia2:::seq_poly

  res <- seq_dissolve(x, buffer = 1)

  a1 <- sum(sf::st_area(x))
  a2 <- sum(sf::st_area(res))

  expect_true(abs(as.numeric(a1 - a2)) < 1e4) # tolérance large
})

test_that("seq_dissolve() snapping modifies geometry", {

  x <- Rsequoia2:::seq_poly

  res1 <- seq_dissolve(x, snapping = FALSE)
  res2 <- seq_dissolve(x, snapping = TRUE)

  expect_false(sf::st_equals(res1, res2, sparse = FALSE)[1,1])
})

test_that("seq_dissolve() is approximately idempotent", {

  x <- Rsequoia2:::seq_poly

  res1 <- seq_dissolve(x)
  res2 <- seq_dissolve(res1)

  diff <- abs(sum(sf::st_area(res1)) - sum(sf::st_area(res2)))

  expect_true(diff < units::set_units(1, "m^2"))
})

test_that("seq_dissolve() handles MULTIPOLYGON", {

  x <- Rsequoia2:::seq_poly |>
    sf::st_cast("MULTIPOLYGON")

  res <- seq_dissolve(x)

  expect_s3_class(res, "sf")
})

test_that("seq_dissolve() handles zero buffer", {

  x <- Rsequoia2:::seq_poly

  res <- seq_dissolve(x, buffer = 0)

  expect_s3_class(res, "sf")
})

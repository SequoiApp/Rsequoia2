test_that("vides_to_line() returns sf LINESTRING", {
  poly <- Rsequoia2:::seq_poly

  out <- vides_to_line(poly) |> quiet()

  expect_s3_class(out, "sf")
  expect_true("geometry" %in% names(out))
  expect_true(inherits(sf::st_geometry(out), "sfc_LINESTRING"))
  expect_true(nrow(out) >= 1)  # ensure we have at least one line
})

test_that("vides_to_line() handles MULTIPOLYGONs", {
  multi <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(
        list(matrix(c(0,0, 0,2, 2,2, 2,0, 0,0), ncol=2, byrow=TRUE)),
        list(matrix(c(3,3, 3,5, 5,5, 5,3, 3,3), ncol=2, byrow=TRUE))
      ))
    ),
    crs = 2154
  )

  out <- vides_to_line(multi)

  expect_s3_class(out, "sf")
  expect_true(all(sf::st_geometry_type(out) %in% c("LINESTRING", "MULTILINESTRING")))
})

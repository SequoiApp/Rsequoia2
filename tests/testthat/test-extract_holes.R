test_that("extract_holes extracts single hole from polygon", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(
      rbind(c(0,0), c(5,0), c(5,5), c(0,5), c(0,0)),   # exterior
      rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))    # hole
    )),
    crs = 4326
  )

  sf_poly <- sf::st_sf(id = 1, geometry = poly)

  holes <- extract_holes(sf_poly)

  expect_s3_class(holes, "sf")
  expect_equal(nrow(holes), 1)
  expect_true(all(sf::st_geometry_type(holes) == "POLYGON"))
  expect_equal(sf::st_crs(holes), sf::st_crs(sf_poly))
})

test_that("extract_holes extracts multiple holes from multipolygon", {
  mp <- sf::st_sfc(
    sf::st_multipolygon(list(
      list(
        rbind(c(0,0), c(5,0), c(5,5), c(0,5), c(0,0)),
        rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))
      ),
      list(
        rbind(c(10,10), c(15,10), c(15,15), c(10,15), c(10,10)),
        rbind(c(11,11), c(12,11), c(12,12), c(11,12), c(11,11))
      )
    )),
    crs = 4326
  )

  sf_mp <- sf::st_sf(id = 1, geometry = mp)

  holes <- extract_holes(sf_mp)

  expect_equal(nrow(holes), 2)
  expect_true(all(sf::st_geometry_type(holes) == "POLYGON"))
})

test_that("extract_holes returns empty sf if no holes", {
  poly <- sf::st_sfc(sf::st_polygon(list(
    rbind(c(0,0), c(5,0), c(5,5), c(0,5), c(0,0))
  )), crs = 4326)

  sf_poly <- sf::st_sf(id = 1, geometry = poly)

  holes <- extract_holes(sf_poly)

  expect_s3_class(holes, "sf")
  expect_equal(nrow(holes), 0)
  expect_equal(sf::st_crs(holes), sf::st_crs(sf_poly))
})

test_that("extract_holes aborts on non-polygon geometries", {
  point <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)
  sf_point <- sf::st_sf(id = 1, geometry = point)

  expect_error(extract_holes(sf_point), "must be of type POLYGON or MULTIPOLYGON")
})

test_that("extract_holes handles multiple polygons with some holes", {
  poly1 <- sf::st_polygon(list(
    rbind(c(0,0), c(5,0), c(5,5), c(0,5), c(0,0)),
    rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))
  ))
  poly2 <- sf::st_polygon(list(
    rbind(c(10,10), c(15,10), c(15,15), c(10,15), c(10,10))
  ))

  sf_poly <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(poly1, poly2, crs = 4326))

  holes <- extract_holes(sf_poly)

  expect_equal(nrow(holes), 1)
  expect_true(all(sf::st_geometry_type(holes) == "POLYGON"))
})

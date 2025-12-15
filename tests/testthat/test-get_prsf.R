test_that("get_prsf() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  # area_sf
  bbox_vals <- c(xmin = 844053.0, ymin = 6831061.6, xmax = 847714.2, ymax = 6833748.3 )
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  # get_toponyme()
  prsf <- get_prsf(area_sf)

  # tests
  expect_s3_class(prsf, "sf")
  expect_true(sf::st_crs(prsf)$epsg == 2154)
})

test_that("get_prsf() returns NULL on area with no forest", {
  skip_on_cran()
  skip_on_ci()

  # empty_area_sf
  empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
  empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

  prsf <- get_prsf(empty_area_sf)

  expect_null(prsf)
})

test_that("get_region() returns sf with expected structure", {
  skip_on_cran()
  skip_on_ci()

  # area_sf
  bbox_vals <- c(
    xmin = 844053.0,
    ymin = 6831061.6,
    xmax = 847714.2,
    ymax = 6833748.3
  )
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  region <- get_region(area_sf, type = "ser")

  expect_s3_class(region, "sf")
  expect_true(nrow(region) > 0)
  expect_true(sf::st_crs(region)$epsg == 2154)
  expect_true(
    all(sf::st_geometry_type(region) %in% c("POLYGON", "MULTIPOLYGON"))
  )
})

test_that("get_region() returns NULL when no region intersects area", {
  skip_on_cran()
  skip_on_ci()

  # empty area
  empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
  empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

  region <- get_region(empty_area_sf, type = "ser")

  expect_null(region)
})

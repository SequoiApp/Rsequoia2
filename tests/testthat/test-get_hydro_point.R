test_that("get_hydro_point works on a small area", {
  skip_on_cran()
  skip_on_ci()

  # Create sf polygon from bbox
  bbox_vals <- c(xmin=547226.9, ymin=6794383.2, xmax=549263.2, ymax=6795983.8)
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  # Run function
  hydro_point <- get_hydro_point(area_sf)

  # Basic checks
  expect_s3_class(hydro_point, "sf")
  expect_true(all(c("TYPE", "NATURE", "NAME", "ROTATION") %in% names(hydro_point)))
  expect_true(sf::st_crs(hydro_point)$epsg == 2154)
  expect_true(all(hydro_point$TYPE %in% c("MAR")))
})

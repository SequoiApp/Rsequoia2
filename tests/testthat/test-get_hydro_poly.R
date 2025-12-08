test_that("get_hydro_poly() works on a small area locally", {
  skip_on_cran()
  skip_on_ci()

  # Create sf polygon from bbox
  bbox_vals <- c(xmin = 547226.9, ymin = 6794383.2, xmax = 549263.2, ymax = 6795983.8)
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  # Run function
  hydro_poly <- get_hydro_poly(area_sf)

  type <- seq_field("type")$name
  nature <- seq_field("nature")$name
  name <- seq_field("name")$name

  # Basic checks
  expect_s3_class(hydro_poly, "sf")
  expect_true(all(c(type, nature, name) %in% names(hydro_poly)))
  expect_true(sf::st_crs(hydro_poly)$epsg == 2154)
  expect_true(all(hydro_poly[[type]] %in% c("SFO", "SFI")))
})

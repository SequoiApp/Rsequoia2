# standard field names ----
type   <- seq_field("type")$name
nature   <- seq_field("nature")$name
name   <- seq_field("name")$name
source <- seq_field("source")$name

test_that("get_toponyme() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  # area_sf
  bbox_vals <- c(xmin = 547226.9, ymin = 6794383.2, xmax = 549263.2, ymax = 6795983.8)
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  # get_toponyme()
  toponyme <- get_toponyme(area_sf)

  # tests
  expect_s3_class(toponyme, "sf")
  expect_true(all(c(type, source, name) %in% names(toponyme)))
  expect_true(sf::st_crs(toponyme)$epsg == 2154)
  expect_true(all(toponyme[[type]] %in% c("TYPON", "THYDR", "TVEGE")))
})

test_that("get_toponyme() returns NULL on area with no forest", {
  skip_on_cran()
  skip_on_ci()

  # empty_area_sf
  empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
  empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

  toponyme <- get_toponyme(empty_area_sf)

  expect_null(toponyme)
})


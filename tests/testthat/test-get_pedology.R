# get_pedolygy test ----
test_that("get_pedology() returns sf with expected fields and values", {
  skip_on_cran()
  skip_on_ci()

  # area_sf
  bbox_vals <- c(xmin = 844053.0, ymin = 6831061.6, xmax = 847714.2, ymax = 6833748.3 )
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  # get_toponyme()
  pedo <- get_pedology(area_sf)

  # tests
  expect_s3_class(pedo, "sf")
  expect_true(all(sf::st_geometry_type(pedo) == "POLYGON"))
  expect_true(sf::st_crs(pedo)$epsg == 2154)
})

test_that("get_pedology() returns NULL on area with no forest", {
  skip_on_cran()
  skip_on_ci()

  # empty_area_sf
  empty_bbox <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  empty_poly <- sf::st_as_sfc(sf::st_bbox(empty_bbox, crs = 2154))
  empty_area_sf <- sf::st_sf(id = 1, geometry = empty_poly)

  pedo <- get_pedology(empty_area_sf)

  expect_null(pedo)
})

# get_pedology_pdf tests ----
test_that("get_pedology_pdf() downloads PDFs from id_ucs", {
  skip_on_cran()
  skip_on_ci()

  # area_sf
  bbox_vals <- c(xmin = 844053.0, ymin = 6831061.6, xmax = 847714.2, ymax = 6833748.3 )
  poly <- sf::st_as_sfc(sf::st_bbox(bbox_vals, crs = 2154))
  area_sf <- sf::st_sf(id = 1, geometry = poly)

  pedo <- get_pedology(area_sf)

  tmp <- file.path(tempdir(), "pedo_pdf")
  dir.create(tmp, showWarnings = FALSE)

  out <- get_pedology_pdf(
    pedology = pedo,
    out_dir  = tmp,
    verbose  = FALSE
  )

  pdfs <- list.files(tmp, pattern = "\\.pdf$", full.names = TRUE)

  expect_true(length(pdfs) > 0)
  expect_true(all(file.exists(pdfs)))
  expect_true(is.character(out))
})

test_that("get_pedology_pdf() returns NULL when no valid id_ucs", {
  skip_on_cran()
  skip_on_ci()

  pedo <- sf::st_sf(
    id = 1,
    id_ucs = NA,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  tmp <- tempdir()

  res <- get_pedology_pdf(
    pedology = pedo,
    out_dir  = tmp,
    verbose  = FALSE
  )

  expect_null(res)
})

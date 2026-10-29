test_that(".climate_transformer() returns clipped climate polygons", {

  climate <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 20,
    ymin = 0,
    ymax = 20,
    crs = "EPSG:2154"
  )

  terra::values(climate) <- c(1, 2, 1, 2)

  levels(climate) <- data.frame(
    value = 1:2,
    label = c("climat océanique", "climat continental")
  )

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(10, 0), c(10, 20),
      c(0, 20), c(0, 0)
    ))),
    crs = 2154
  )

  result <- .climate_transformer(x, climate)

  expect_s3_class(result, "sf")
  expect_gt(nrow(result), 0)
  expect_true("label" %in% names(result))
  expect_true(all(sf::st_geometry_type(result) == "POLYGON"))
  expect_true(all(sf::st_covered_by(result, x, sparse = FALSE)))
})


test_that(".climate_transformer() validates its arguments", {

  climate <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:2154"
  )

  expect_error(
    .climate_transformer(data.frame(), climate),
    "class"
  )

  expect_error(
    .climate_transformer(
      sf::st_sfc(sf::st_point(), crs = 2154),
      climate
    ),
    "empty"
  )

  expect_error(
    .climate_transformer(
      sf::st_sfc(crs = 2154),
      data.frame()
    ),
    "SpatRaster"
  )
})


test_that(".climate_transformer() errors when vectorization produces no polygons", {

  climate <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:2154"
  )

  terra::values(climate) <- NA

  x <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1),
      c(0, 1), c(0, 0)
    ))),
    crs = 2154
  )

  expect_error(
    .climate_transformer(x, climate),
    "Vectorization failed"
  )
})

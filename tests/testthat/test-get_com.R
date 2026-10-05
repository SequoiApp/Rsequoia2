test_that("get_com() returns commune polygons", {

  local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  com <- get_com(Rsequoia2:::seq_poly, verbose = FALSE)

  expect_s3_class(com, "sf")
  expect_gt(nrow(com), 0)
  expect_true(
    all(sf::st_geometry_type(com) %in% c("POLYGON", "MULTIPOLYGON"))
  )
})


test_that("derive_com() derives line and point geometries", {

  poly <- Rsequoia2:::seq_poly

  line <- Rsequoia2:::derive_com(poly, "line")
  point <- Rsequoia2:::derive_com(poly, "point")

  expect_s3_class(line, "sf")
  expect_s3_class(point, "sf")

  expect_true(
    all(sf::st_geometry_type(line) %in% c("LINESTRING", "MULTILINESTRING"))
  )

  expect_true(
    all(sf::st_geometry_type(point) == "POINT")
  )
})


test_that("clip_com() clips commune geometries", {

  poly <- Rsequoia2:::seq_poly

  clip <- sf::st_bbox(poly) |>
    sf::st_as_sfc()

  out <- Rsequoia2:::clip_com(
    poly,
    clip,
    verbose = FALSE
  )

  expect_s3_class(out, "sf")
  expect_gt(nrow(out), 0)
})

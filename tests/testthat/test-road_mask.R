test_that("road_mask() ignores lines with TYPE == 'PN'", {
  x <- sf::st_sf(
    TYPE = c("PN", "RN"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0, 1,1), ncol=2, byrow=TRUE)),
      sf::st_linestring(matrix(c(0,0, 2,2), ncol=2, byrow=TRUE)),
      crs = 2154
    )
  )

  out <- road_mask(x, dist = 1) |> quiet()

  expect_s3_class(out, "sf")
  # PN line should be ignored
  coords_out <- sf::st_coordinates(sf::st_cast(out, "POINT"))
  expect_false(any(apply(coords_out, 1, function(pt) all(pt == c(1,1)))))
})

test_that("road_mask() returns a polygon sf covering lines and duplicate points", {
  x <- sf::st_sf(
    TYPE = c("RN", "RC"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0, 0,5), ncol=2, byrow=TRUE)),
      sf::st_linestring(matrix(c(0,5, 0,10), ncol=2, byrow=TRUE))
    ),
    crs = 2154
  )

  out <- road_mask(x, dist = 2) |> quiet()

  expect_s3_class(out, "sf")
  expect_true(inherits(sf::st_geometry(out), "sfc_POLYGON"))
  # ensure output is non-empty
  expect_gt(as.numeric(sf::st_area(out)), 0)
})

test_that("road_mask() handles points with Z coordinate", {
  x <- sf::st_sf(
    TYPE = "RN",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0,1,1,2,2), ncol=3, byrow=TRUE))
    ),
    crs = 2154
  )

  out <- road_mask(x, dist = 1) |> quiet()
  expect_s3_class(out, "sf")
  expect_true(all(sf::st_geometry_type(out) %in% c("POLYGON","MULTIPOLYGON")))
})

test_that("road_mask() handles duplicated points correctly", {
  x <- sf::st_sf(
    TYPE = "RC",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0, 1,1, 1,1, 2,2), ncol=2, byrow=TRUE))
    ),
    crs = 2154
  )

  out <- road_mask(x, dist = 0.5) |> quiet()
  expect_s3_class(out, "sf")
  # output polygon must cover all original points
  pts <- sf::st_cast(x, "POINT", warn = F)
  expect_true(all(sf::st_intersects(pts, out, sparse = FALSE)))
})

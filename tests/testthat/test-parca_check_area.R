test_that("parca_check_area returns no inconsistencies when areas match", {
  skip_if_not_installed("sf")
  library(sf)

  poly1 <- st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0)))) # 100 m2
  poly2 <- st_polygon(list(rbind(c(0,0), c(20,0), c(20,20), c(0,20), c(0,0)))) # 400 m2

  parca <- st_sf(
    IDU        = c("A", "B"),
    CONTENANCE = c(100, 400),  # in m2
    geometry   = st_sfc(poly1, poly2),
    crs        = 2154
  )

  result <- parca_check_area(parca, verbose = FALSE)

  expect_s3_class(result, "sf")
  expect_true(all(result$AREA_INCONSISTENT == FALSE))
  expect_equal(result$AREA_DIFFERENCE_M2, c(0, 0))
  expect_equal(result$AREA_DIFFERENCE_PCT, c(0, 0))
})

test_that("parca_check_area detects inconsistent areas correctly (AND logic)", {
  skip_if_not_installed("sf")
  library(sf)

  poly1 <- st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10), c(0,0)))) # 100 m2
  poly2 <- st_polygon(list(rbind(c(0,0), c(20,0), c(20,20), c(0,20), c(0,0)))) # 400 m2
  poly3 <- st_polygon(list(rbind(c(0,0), c(40,0), c(40,40), c(0,40), c(0,0)))) # 1600 m2

  parca <- st_sf(
    IDU        = c("A", "B", "C"),
    CONTENANCE = c(100, 400, 1000),  # C is inconsistent (1000 vs 1600)
    geometry   = st_sfc(poly1, poly2, poly3),
    crs        = 2154
  )

  result <- parca_check_area(parca, verbose = FALSE)

  # Check class and columns
  expect_s3_class(result, "sf")
  expect_true(all(c(
    "AREA_SIG",
    "AREA_DIFFERENCE_M2",
    "AREA_DIFFERENCE_PCT",
    "AREA_INCONSISTENT"
  ) %in% names(result)))

  # AREA_SIG in ha
  expect_equal(result$AREA_SIG, c(0.01, 0.04, 0.16), tolerance = 1e-6)

  # Difference in m2
  expect_equal(result$AREA_DIFFERENCE_M2, c(0, 0, 600))

  # Percent difference
  expect_equal(result$AREA_DIFFERENCE_PCT[3], 600 / 1000, tolerance = 1e-6)

  # Only C is inconsistent
  expect_identical(result$AREA_INCONSISTENT, c(FALSE, FALSE, TRUE))
})

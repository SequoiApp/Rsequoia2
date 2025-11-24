test_that("ua_update_surfcad updates inconsistent surfaces", {
  skip_if_not_installed("sf")
  library(sf)

  ua <- st_sf(
    IDU = c("A", "B", "C"),
    SURF_CA = c(100, 200, 300),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
  )

  parca <- st_sf(
    IDU = c("A", "B", "C"),
    SURF_CA = c(100, 250, 300),  # B is different
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
  )

  out <- ua_update_surfcad(ua, parca, verbose = FALSE)
  expect_equal(out$SURF_CA, c(100, 250, 300))
})

test_that("ua_update_surfcad keeps UA unchanged when no difference", {
  skip_if_not_installed("sf")
  library(sf)

  ua <- st_sf(
    IDU = c("A", "B"),
    SURF_CA = c(100, 200),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )
  parca <- ua
  out <- ua_update_surfcad(ua, parca, verbose = FALSE)
  expect_identical(out$SURF_CA, ua$SURF_CA)
})

test_that("ua_update_surfcad prints a warning when corrections are made (verbose)", {
  skip_if_not_installed("sf")
  library(sf)

  ua <- st_sf(IDU = "A", SURF_CA = 100, geometry = st_sfc(st_point(c(0,0))))
  parca <- st_sf(IDU = "A", SURF_CA = 150, geometry = st_sfc(st_point(c(0,0))))

  expect_message(
    ua_update_surfcad(ua, parca, verbose = TRUE),
    "corrected"
  )
})

test_that("ua_update_surfcad prints success message when no correction", {
  skip_if_not_installed("sf")
  library(sf)

  ua <- st_sf(IDU = "A", SURF_CA = 100, geometry = st_sfc(st_point(c(0,0))))
  parca <- ua

  expect_message(
    ua_update_surfcad(ua, parca, verbose = TRUE),
    "No cadastral area discrepancies detected"
  )
})

test_that("ua_update_surfcad ignores NA values from parca", {
  skip_if_not_installed("sf")
  library(sf)

  ua <- st_sf(
    IDU = c("A", "B"),
    SURF_CA = c(100, 200),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  parca <- st_sf(
    IDU = c("A", "B"),
    SURF_CA = c(150, NA),  # B is NA → must not replace
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  out <- ua_update_surfcad(ua, parca, verbose = FALSE)
  expect_equal(out$SURF_CA, c(150, 200))
})


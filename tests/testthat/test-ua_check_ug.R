test_that("ua_check_ug() works with consistent UG", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  expect_no_warning(res <- ua_check_ug(ua, verbose = FALSE))
  expect_true(res)
})

test_that("ua_check_ug() detects inconsistent UG", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "PFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  expect_warning(
    res <- ua_check_ug(ua, verbose = FALSE),
    "inconsistent UG"
  )
  expect_false(res)
})

test_that("ua_check_ug() handles multiple UGs", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 10, 5, 5),
    PLT_PLMT = c("MFT", "MFT", "PFT", "PFT"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      sf::st_point(c(3, 3))
    )
  )

  expect_no_warning(res <- ua_check_ug(ua, verbose = FALSE))
  expect_true(res)
})

test_that("ua_check_ug() detects multiple inconsistent UGs", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 10, 5, 5),
    PLT_PLMT = c("MFT", "MTT", "PFT", "PTT"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      sf::st_point(c(3, 3))
    )
  )

  expect_warning(
    res <- ua_check_ug(ua, verbose = FALSE),
    "inconsistent UG"
  )
  expect_false(res)
})

test_that("ua_check_ug() works with NA values", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c(NA, "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  expect_warning(
    res <- ua_check_ug(ua, verbose = FALSE),
    "inconsistent UG"
  )
  expect_false(res)
})

test_that("ua_check_ug() throws message wehn verbose = TRUE", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  expect_message(
    res <- ua_check_ug(ua, verbose = TRUE),
    "All UG are consistent."
  )
  expect_true(res)
})

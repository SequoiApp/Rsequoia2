test_that("ua_clean_ug fixes minor inconsistencies", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "A3"),
    PARFOR = c("UG1", "UG1", "UG1"),
    SURF_COR = c(10, 0.4, 10),
    PLT_PLMT = c("MFT", "PFT", "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)), sf::st_point(c(2,2)))
  )

  cleaned <- ua_clean_ug(ua, ug_keys = "peuplement", atol = 1, rtol = 0.05, verbose = FALSE)

  # The minor row should be corrected to match dominant
  expect_equal(cleaned$PLT_PLMT, c("MFT", "MFT", "MFT"))
  expect_true(all(cleaned$ug_valid))
})

test_that("ua_clean_ug does not change major inconsistent rows", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "PFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  cleaned <- ua_clean_ug(ua, ug_keys = "peuplement", atol = 1, rtol = 0.05, verbose = FALSE)

  # Major difference remains
  expect_false(all(cleaned$ug_valid))
  expect_equal(cleaned$PLT_PLMT, c("MFT", "PFT"))
})

test_that("ua_clean_ug handles multiple UGs independently", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 0.4, 5, 0.2),
    PLT_PLMT = c("MFT", "PFT", "PFT", "PPT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)),
                      sf::st_point(c(2,2)), sf::st_point(c(3,3)))
  )

  cleaned <- ua_clean_ug(ua, ug_keys = "peuplement", atol = 1, rtol = 0.05, verbose = FALSE)

  # Minor rows corrected independently per UG
  expect_equal(cleaned$PLT_PLMT, c("MFT", "MFT", "PFT", "PFT"))
})

test_that("ua_clean_ug keeps ug_valid column as logical", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = "A1",
    PARFOR = "UG1",
    SURF_COR = 10,
    PLT_PLMT = "MFT",
    geometry = sf::st_sfc(sf::st_point(c(0,0)))
  )

  cleaned <- ua_clean_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_true("ug_valid" %in% colnames(cleaned))
  expect_true(is.logical(cleaned$ug_valid))
})

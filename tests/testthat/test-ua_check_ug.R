test_that("ua_check_ug marks consistent UG correctly", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, "peuplement", verbose = FALSE)

  expect_true(all(checked$ug_valid))
})

test_that("ua_check_ug detects inconsistent UG", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "PFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
  expect_equal(unique(checked$PARFOR[!checked$ug_valid]), "UG1")
})

test_that("ua_check_ug handles multiple UGs", {
  skip_if_not_installed("sf")

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

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_true(all(checked$ug_valid))
})

test_that("ua_check_ug handles multiple inconsistent UGs", {
  skip_if_not_installed("sf")

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

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
})

test_that("ua_check_ug works with NA values", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c(NA, "MFT"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
})

test_that("ua_check_ug returns ug_valid column", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = "A1",
    PARFOR = "UG1",
    SURF_COR = 10,
    PLT_PLMT = "MFT",
    geometry = sf::st_sfc(sf::st_point(c(0,0)))
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_true("ug_valid" %in% colnames(checked))
  expect_true(is.logical(checked$ug_valid))
})

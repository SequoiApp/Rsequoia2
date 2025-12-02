test_that("ua_clean_ug() fixes minor inconsistencies", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "A3"),
    PARFOR = c("UG1", "UG1", "UG1"),
    SURF_COR = c(10, 0.4, 10),
    PLT_PLMT = c("MFT", "PFT", "MFT"),
    geometry = sf::st_sfc(replicate(3, sf::st_point(), FALSE))
  )

  cleaned <- ua_clean_ug(ua, atol = 1, rtol = 0.05)

  # The minor row should be corrected to match dominant
  expect_all_equal(cleaned$PLT_PLMT, "MFT")
})

test_that("ua_clean_ug() does not change major inconsistent rows", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 5),
    PLT_PLMT = c("MFT", "PFT"),
    geometry = sf::st_sfc(replicate(2, sf::st_point(), FALSE))
  )

  cleaned <- ua_clean_ug(ua, atol = 1, rtol = 0.05)

  expect_equal(cleaned$PLT_PLMT, c("MFT", "PFT"))
})

test_that("ua_clean_ug() handles multiple UGs independently", {

  ua <- sf::st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 0.6, 5, 0.2),
    PLT_PLMT = c("MFT", "PFT", "PFT", "PPT"),
    geometry = sf::st_sfc(replicate(4, sf::st_point(), FALSE))
  )

  cleaned <- ua_clean_ug(ua, atol = 1, rtol = 0.05)

  # Minor rows corrected independently per UG
  expect_equal(cleaned$PLT_PLMT, c("MFT", "MFT", "PFT", "PFT"))
})

test_that("ua_clean_ug() uses the largest description as reference", {
  ua <- sf::st_sf(
    PARFOR = c("UG1","UG1","UG1"),
    SURF_COR = c(0.4, 10, 0.3),
    PLT_PLMT = c("BAD", "GOOD", "BAD"),
    geometry = sf::st_sfc(replicate(3, sf::st_point(), FALSE))
  )
  cleaned <- ua_clean_ug(ua, atol = 1)
  expect_equal(cleaned$PLT_PLMT, rep("GOOD", 3))
})

test_that("ua_clean_ug() works when all rows are small", {
  ua <- sf::st_sf(
    PARFOR = c("UG1","UG1"),
    SURF_COR = c(0.2, 0.3),
    PLT_PLMT = c("A", "B"),
    geometry = sf::st_sfc(replicate(2, sf::st_point(), FALSE))
  )
  cleaned <- ua_clean_ug(ua, atol = 1)
  expect_equal(cleaned$PLT_PLMT, c("B","B"))
})

test_that("ua_clean_ug() works when only one small row", {
  ua <- sf::st_sf(
    PARFOR = c("UG1"),
    SURF_COR = c(0.2),
    PLT_PLMT = c("A"),
    geometry = sf::st_sfc(sf::st_point())
  )
  cleaned <- ua_clean_ug(ua, atol = 1)
  expect_equal(cleaned$PLT_PLMT, c("A"))
})


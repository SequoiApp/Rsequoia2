test_that("ua_check_ug marks consistent UG correctly", {
  ua <- st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "MFT"),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, "peuplement", verbose = FALSE)

  expect_true(all(checked$ug_valid))
})

test_that("ua_check_ug detects inconsistent UG", {
  ua <- st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c("MFT", "PFT"),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
  expect_equal(unique(checked$PARFOR[!checked$ug_valid]), "UG1")
})

test_that("ua_check_ug handles multiple UGs", {

  ua <- st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 10, 5, 5),
    PLT_PLMT = c("MFT", "MFT", "PFT", "PFT"),
    geometry = st_sfc(
      st_point(c(0, 0)),
      st_point(c(1, 1)),
      st_point(c(2, 2)),
      st_point(c(3, 3))
    )
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_true(all(checked$ug_valid))
})

test_that("ua_check_ug handles multiple inconsistent UGs", {

  ua <- st_sf(
    IDU = c("A1", "A2", "B1", "B2"),
    PARFOR = c("UG1", "UG1", "UG2", "UG2"),
    SURF_COR = c(10, 10, 5, 5),
    PLT_PLMT = c("MFT", "MTT", "PFT", "PTT"),
    geometry = st_sfc(
      st_point(c(0, 0)),
      st_point(c(1, 1)),
      st_point(c(2, 2)),
      st_point(c(3, 3))
    )
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
})

test_that("ua_check_ug works with NA values", {
  ua <- st_sf(
    IDU = c("A1", "A2"),
    PARFOR = c("UG1", "UG1"),
    SURF_COR = c(10, 10),
    PLT_PLMT = c(NA, "MFT"),
    geometry = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_false(all(checked$ug_valid))
})

test_that("ua_check_ug returns ug_valid column", {
  ua <- st_sf(
    IDU = "A1",
    PARFOR = "UG1",
    SURF_COR = 10,
    PLT_PLMT = "MFT",
    geometry = st_sfc(st_point(c(0,0)))
  )

  checked <- ua_check_ug(ua, ug_keys = "peuplement", verbose = FALSE)

  expect_true("ug_valid" %in% colnames(checked))
  expect_true(is.logical(checked$ug_valid))
})

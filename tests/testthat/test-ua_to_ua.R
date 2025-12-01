test_that("ua_to_ua returns UA unchanged and warns if IDU invalid", {
  skip_if_not_installed("sf")

  ua <- sf::st_sf(
    IDU = c("A", "B"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  parca <- sf::st_sf(
    IDU = c("X", "Y"),
    geometry = sf::st_sfc(sf::st_point(c(2,2)), sf::st_point(c(3,3)))
  )

  warnings <- testthat::capture_warnings(
    res <- ua_to_ua(ua, parca, verbose = FALSE)
  )

  expect_true(any(grepl("IDUs of the PARCA layer", warnings)))
  expect_identical(res, ua)
})

test_that("ua_to_ua runs full workflow for valid inputs", {
  skip_if_not_installed("sf")

  # Minimal valid UA
  ua <- sf::st_sf(
    IDU = c("A", "A", "B"),
    N_PARFOR = c(1, 1, 1),
    N_SSPARFOR = c(1, 1, 1),
    PLT_PLMT = c("A", "A", "A"),
    SURF_CA = c(10, 10, 20),
    geometry = sf::st_sfc(
      sf::st_point(c(0,0)),
      sf::st_point(c(1,1)),
      sf::st_point(c(2,2))
    )
  ) |> seq_normalize("ua")

  # Matching PARCA
  parca <- sf::st_sf(
    IDU = c("A", "B"),
    SURF_CA = c(10, 20),
    geometry = sf::st_sfc(
      sf::st_point(c(0,0)),
      sf::st_point(c(1,1))
    )
  )

  res <- ua_to_ua(ua, parca, verbose = FALSE)

  # Still an sf
  expect_s3_class(res, "sf")

  # Same rows
  expect_equal(nrow(res), 3)

  # UG must exist (produced by ua_generate_ug)
  expect_true("PARFOR" %in% names(res) || "ug" %in% tolower(names(res)))

  # Corrected area exists
  expect_true(any(grepl("SURF_COR", names(res), ignore.case = TRUE)))

  # ug_valid exists
  expect_true("ug_valid" %in% names(res))

  # ug_valid is logical
  expect_type(res$ug_valid, "logical")
})

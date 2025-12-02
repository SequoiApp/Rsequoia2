test_that("ua_to_ua() abort when IDU are invalid", {

  parca <- sf::st_sf(
    IDU = c("X", "Y"),
    SURF_CA = c(10, 10),
    geometry = sf::st_sfc(sf::st_point(c(2,2)), sf::st_point(c(3,3)))
  )

  ua <- sf::st_sf(
    IDU = c("A", "B"),
    SURF_CA = c(10, 10),
    N_PARFOR = c(1, 1),
    N_SSPARFOR = c(1, 1),
    PLT_PLMT = c("MFT", "MFT"),
    PLT_RICH = c("RICHE", "RICHE"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1)))
  )

  expect_error(
    ua_to_ua(ua, parca, verbose = FALSE),
    "Please correct IDU inconsistency before going further"
  ) |> suppressWarnings()

})

test_that("ua_to_ua() runs full workflow for valid inputs", {

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

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 3)
  expect_true("PARFOR" %in% names(res) || "ug" %in% tolower(names(res)))
  expect_true(any(grepl("SURF_COR", names(res), ignore.case = TRUE)))
})

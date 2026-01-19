test_that("update_infra remaps known values correctly", {

  x <- rbind(Rsequoia2:::seq_poly,
              Rsequoia2:::seq_poly)
  x$type <- c("BT", "SURFO", "RUi", "RUI", "VEG", "LE")

  res <- update_infra(x)

  expect_equal(
    res$TYPE,
    c("BAT", "SFP", "RUI", "RUP", "FOR", "LEL")
  )
})

test_that("update_infra keeps unmapped values unchanged", {

  x <- Rsequoia2:::seq_poly
  x$type <- c("ABC", "DEF", "GHI")

  res <- update_infra(x)

  expect_equal(
    res$TYPE,
    c("ABC", "DEF", "GHI")
  )
})

test_that("update_infra preserves sf structure", {

  x <- Rsequoia2:::seq_poly

  res <- update_infra(x)

  expect_s3_class(res, "sf")
  expect_equal(sf::st_geometry(res), sf::st_geometry(x))
})

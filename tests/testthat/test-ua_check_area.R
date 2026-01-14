test_that("ua_check_area() updates inconsistent surfaces with warning", {

  parca <- fake_parca(1:3, 1:3, 1:3, 1:3, contenance = c(1, 2, 3))
  ua <- seq_normalize(fake_parca(1:3, 1:3, 1:3, 1:3, contenance = c(1, 4, 5)), "ua")

  expect_warning(
    res <- ua_check_area(ua, parca),
    "2 cadastral area values corrected in UA"
  )

  cad_area <- seq_field("cad_area")$name
  expect_equal(res[[cad_area]], c(1, 2, 3))
})

test_that("ua_check_area() keeps ua unchanged when no difference", {

  parca <- fake_parca()
  ua <- seq_normalize(parca, "ua")
  expect_no_warning(res <- ua_check_area(ua, parca, verbose = FALSE))

  cad_area <- seq_field("cad_area")$name
  expect_identical(res[[cad_area]], ua[[cad_area]])
})

test_that("ua_check_area() prints success message when verbose = TRUE", {

  parca <- fake_parca()
  ua <- seq_normalize(parca, "ua")

  expect_message(
    ua_check_area(ua, parca, verbose = TRUE),
    "No cadastral area discrepancies detected"
  )
})

test_that("ua_check_area() ignores NA values from parca", {

  parca <- fake_parca(1:3, 1:3, 1:3, 1:3, contenance = c(NA, 2, NA))
  ua <- seq_normalize(fake_parca(1:3, 1:3, 1:3, 1:3, contenance = c(1, 2, 3)), "ua")

  res <- ua_check_area(ua, parca, verbose = FALSE) |> suppressWarnings()

  cad_area <- seq_field("cad_area")$name
  expect_equal(res[[cad_area]], c(1, 2, 3))
})


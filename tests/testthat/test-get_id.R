test_that("get_id() returns correct ID (single value)", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- fake_matrice(id = c("ABC", "ABC", "ABC"))

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_invisible(expect_equal(get_id(d), "ABC"))

})

test_that("get_id() verbose mode prints info", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- fake_matrice(id = c("ABC", "ABC", "ABC"))

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_message(get_id(d, verbose = TRUE), "Detected forest ID")

})

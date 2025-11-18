test_that("get_id() returns correct ID (single value)", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- data.frame(IDENTIFIANT = c("ABC", "ABC", "ABC"))
  m$PROPRIETAIRE <- NA
  m$INSEE <- NA
  m$PREFIXE <- NA
  m$SECTION <- NA
  m$NUMERO <- NA
  m$LIEU_DIT <- NA
  m$TX_BOISEE <- NA

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_invisible(expect_equal(get_id(d), "ABC"))

})

test_that("get_id() verbose mode prints info", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- data.frame(IDENTIFIANT = c("ABC", "ABC", "ABC"))
  m$PROPRIETAIRE <- NA
  m$INSEE <- NA
  m$PREFIXE <- NA
  m$SECTION <- NA
  m$NUMERO <- NA
  m$LIEU_DIT <- NA
  m$TX_BOISEE <- NA

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_message(get_id(d, verbose = TRUE), "Detected forest ID")

})

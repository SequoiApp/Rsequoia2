test_that("read_matrice() errors when no *_matrice.xlsx file", {
  d <- tempdir()
  expect_error(
    read_matrice(d),
    "See .*Rsequoia2::create_matrice.* to generate one"
  )
})

test_that("read_matrice() errors when multiple *_matrice.xlsx files", {
  d <- tempdir()
  m1 <- file.path(d, "1_matrice.xlsx")
  openxlsx2::write_xlsx(fake_matrice(id = "X1"), m1)

  m2 <- file.path(d, "2_matrice.xlsx")
  openxlsx2::write_xlsx(fake_matrice(id = "X2"), m2)

  on.exit(unlink(c(m1, m2)))

  expect_error(
    read_matrice(d),
    "Multiple .*_matrice.xlsx.* files"
  )
})

test_that("read_matrice() errors when column(s) missing", {
  d <- tempdir()

  m <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(data.frame(OTHER = 1), m)
  on.exit(unlink(m))

  expect_error(read_matrice(d), "Missing column in")

})


test_that("read_matrice() errors when IDENTIFIANT is empty", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- fake_matrice(id = c("", NA, " "))

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_error(read_matrice(d), "IDENTIFIANT.*empty")
})

test_that("read_matrice() errors when multiple IDENTIFIANT values", {
  d <- tempdir()

  f <- file.path(d, "test_matrice.xlsx")
  m <- fake_matrice(id = c("A", "B"))

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_error(read_matrice(d), "Multiple IDs detected")

})

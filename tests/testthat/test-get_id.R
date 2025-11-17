test_that("get_id() errors when no *_matrice.xlsx file", {
  d <- tempdir()
  expect_error(
    get_id(d),
    "See .*Rsequoia2::create_matrice.* to generate one"
  )
})

test_that("get_id() errors when multiple *_matrice.xlsx files", {
  d <- tempdir()
  f1 <- file.path(d, "A_matrice.xlsx")
  f2 <- file.path(d, "B_matrice.xlsx")

  openxlsx2::write_xlsx(data.frame(IDENTIFIANT = "X1"), f1)
  openxlsx2::write_xlsx(data.frame(IDENTIFIANT = "X2"), f2)

  expect_error(
    get_id(d),
    "Multiple .*_matrice.xlsx.* files"
  )

  unlink(c(f1, f2))
})

test_that("get_id() errors when IDENTIFIANT column missing", {
  d <- tempdir()
  f <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(data.frame(OTHER = 1), f)

  expect_error(
    get_id(d),
    "IDENTIFIANT.*missing"
  )

  unlink(f)
})

test_that("get_id() errors when IDENTIFIANT is empty", {
  d <- tempdir()
  f <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(data.frame(IDENTIFIANT = c("", NA, " ")), f)

  expect_error(
    get_id(d),
    "IDENTIFIANT.*empty"
  )

  unlink(f)
})

test_that("get_id() errors when multiple IDENTIFIANT values", {
  d <- tempdir()
  f <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(
    data.frame(IDENTIFIANT = c("A", "B")),
    f
  )

  expect_error(
    get_id(d),
    "Multiple IDs detected"
  )

  unlink(f)
})

test_that("get_id() returns correct ID (single value)", {
  d <- tempdir()
  f <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(
    data.frame(IDENTIFIANT = c("ABC", "ABC", "ABC")),
    f
  )

  expect_invisible(
    expect_equal(get_id(d), "ABC")
  )

  unlink(f)
})

test_that("verbose mode prints info", {
  d <- tempdir()
  f <- file.path(d, "test_matrice.xlsx")
  openxlsx2::write_xlsx(
    data.frame(IDENTIFIANT = "XYZ"),
    f
  )

  expect_message(
    get_id(d, verbose = TRUE),
    "Detected forest ID"
  )

  unlink(f)
})

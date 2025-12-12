test_that("read_matrice() errors when no *_matrice.xlsx file", {
  expect_error(
    read_matrice(tempdir()),
    "See .*Rsequoia2::create_matrice.* to generate one"
  )
})

test_that("read_matrice() errors when multiple *_matrice.xlsx files", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m1 <- file.path(seq_cache, "1_matrice.xlsx")
  openxlsx2::write_xlsx(fake_matrice(id = "X1"), m1)

  m2 <- file.path(seq_cache, "2_matrice.xlsx")
  openxlsx2::write_xlsx(fake_matrice(id = "X2"), m2)

  expect_error(
    read_matrice(seq_cache),
    "Multiple .*_matrice.xlsx.* files"
  )
})

test_that("read_matrice() errors when column(s) missing", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- file.path(seq_cache, "test_matrice.xlsx")
  openxlsx2::write_xlsx(data.frame(OTHER = 1), m)

  expect_error(read_matrice(seq_cache), "Missing column in")

})


test_that("read_matrice() errors when IDENTIFIANT is empty", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  f <- file.path(seq_cache, "test_matrice.xlsx")
  m <- fake_matrice(id = c("", NA, " "))

  openxlsx2::write_xlsx(m, f)

  expect_error(read_matrice(seq_cache), "IDENTIFIANT.*empty")
})

test_that("read_matrice() errors when multiple IDENTIFIANT values", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  f <- file.path(seq_cache, "test_matrice.xlsx")
  m <- fake_matrice(id = c("A", "B"))

  openxlsx2::write_xlsx(m, f)
  on.exit(unlink(f))

  expect_error(read_matrice(seq_cache), "Multiple IDs detected")

})

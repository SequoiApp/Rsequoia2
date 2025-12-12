test_that("get_id() returns correct ID (single value)", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- fake_matrice(id = c("ABC", "ABC", "ABC"))
  openxlsx2::write_xlsx(m, file.path(seq_cache, "test_matrice.xlsx"))

  expect_invisible(expect_equal(get_id(seq_cache), "ABC"))

})

test_that("get_id() verbose mode prints info", {
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- fake_matrice(id = c("ABC", "ABC", "ABC"))
  openxlsx2::write_xlsx(m, file.path(seq_cache, "test_matrice.xlsx"))

  expect_message(get_id(seq_cache, verbose = TRUE), "Detected forest ID")

})

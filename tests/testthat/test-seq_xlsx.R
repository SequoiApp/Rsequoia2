test_that("seq_xlsx() creates an Excel file with correct sheets", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  tables <- list(
    "iris" = head(iris),
    "mtcars" = head(mtcars)
  )

  seq_xlsx(tables, tmp, verbose = FALSE)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # read back and check sheet names
  wb <- openxlsx2::wb_load(tmp)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  expect_setequal(sheets, names(tables))
})

test_that("seq_xlsx() adds .xlsx extension if missing", {
  tmp <- tempfile()
  on.exit(unlink(paste0(tmp, ".xlsx")), add = TRUE)

  seq_xlsx(list(test = head(iris)), tmp, verbose = FALSE)
  expect_true(file.exists(paste0(tmp, ".xlsx")))
})

test_that("seq_xlsx() refuses to overwrite existing file by default", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  seq_xlsx(list(a = head(iris)), tmp, verbose = FALSE)
  expect_error(
    seq_xlsx(list(b = head(mtcars)), tmp, verbose = FALSE),
    "File already exists"
  )
})

test_that("seq_xlsx() overwrites when requested", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  seq_xlsx(list(a = head(iris)), tmp, verbose = FALSE)
  expect_silent(
    seq_xlsx(list(b = head(mtcars)), tmp, overwrite = TRUE, verbose = FALSE)
  )
})

test_that("seq_xlsx() fails gracefully if directory does not exist", {
  bad_path <- file.path(tempdir(), "nonexistent_dir", "file.xlsx")
  expect_error(
    seq_xlsx(list(test = head(iris)), bad_path, verbose = FALSE),
    "does not exist"
  )
})

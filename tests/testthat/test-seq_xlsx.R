test_that("seq_xlsx() creates an Excel file with named arg", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  seq_xlsx(
    iris = head(iris),
    mtcars = head(mtcars),
    filename = tmp,
    verbose = FALSE
  )

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # read back and check sheet names
  wb <- openxlsx2::wb_load(tmp)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  expect_setequal(sheets, c("iris", "mtcars"))
})

test_that("seq_xlsx() creates an Excel file with unnamed arg", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  sheet1 = head(iris)
  sheet2 = head(mtcars)

  seq_xlsx(
    sheet1, sheet2,
    filename = tmp,
    verbose = FALSE
  )

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)

  # read back and check sheet names
  wb <- openxlsx2::wb_load(tmp)
  sheets <- openxlsx2::wb_get_sheet_names(wb)
  expect_setequal(sheets, c("sheet1", "sheet2"))
})

test_that("seq_xlsx() adds .xlsx extension if missing", {
  tmp <- tempfile()
  on.exit(unlink(paste0(tmp, ".xlsx")), add = TRUE)

  seq_xlsx(test = head(iris), filename = tmp, verbose = FALSE)
  expect_true(file.exists(paste0(tmp, ".xlsx")))
})

test_that("seq_xlsx() refuses to overwrite existing file by default", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  seq_xlsx(a = head(iris), filename = tmp, verbose = FALSE)
  expect_error(
    seq_xlsx(b = head(mtcars), filename = tmp, verbose = FALSE),
    "File already exists"
  )
})

test_that("seq_xlsx() overwrites when requested", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  seq_xlsx(a = head(iris), filename = tmp, verbose = FALSE)
  expect_silent(
    seq_xlsx(b = head(mtcars), filename = tmp, overwrite = TRUE, verbose = FALSE)
  )
})


test_that("seq_xlsx() fails gracefully if x not a data.frame", {
  expect_error(
    seq_xlsx(list(), "tables must be a named")
  )
})

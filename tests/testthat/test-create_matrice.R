test_that("create_matrice() creates a valid Excel file", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  # should create file
  create_matrice(tmp, verbose = FALSE)
  expect_true(file.exists(tmp))

  # file should not be empty
  expect_gt(file.size(tmp), 0)
})

test_that("create_matrice() adds .xlsx extension if missing", {
  tmp <- tempfile()  # no extension
  on.exit(unlink(paste0(tmp, ".xlsx")), add = TRUE)

  create_matrice(tmp, verbose = FALSE)
  expect_true(file.exists(paste0(tmp, ".xlsx")))
})

test_that("create_matrice() refuses to overwrite by default", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  create_matrice(tmp, verbose = FALSE)
  expect_error(
    create_matrice(tmp, verbose = FALSE),
    "File already exists"
  )
})

test_that("create_matrice() overwrites when requested", {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  create_matrice(tmp, verbose = FALSE)
  expect_silent(create_matrice(tmp, overwrite = TRUE, verbose = FALSE))
})

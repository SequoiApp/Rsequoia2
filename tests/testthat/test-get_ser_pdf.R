test_that("get_ser_pdf() errors when codeser is missing", {

  ser <- data.frame(foo = 1:3)

  expect_error(
    get_ser_pdf(ser, verbose = FALSE),
    "codeser"
  )
})

test_that("get_ser_pdf() returns NULL when no valid codeser", {

  ser <- data.frame(codeser = c(NA, NA))

  res <- get_ser_pdf(ser, verbose = FALSE)

  expect_null(res)
})

test_that("get_ser_pdf() creates output directory if missing", {

  ser <- data.frame(codeser = "A12")
  tmp <- tempfile()

  local_mocked_bindings(
    download.file = function(...) NULL,
    .package = "utils"
  )

  expect_false(dir.exists(tmp))

  get_ser_pdf(ser, out_dir = tmp, verbose = FALSE)

  expect_true(dir.exists(tmp))
})

test_that("get_ser_pdf() skips download if file exists and overwrite = FALSE", {

  ser <- data.frame(codeser = "B34")
  tmp <- tempdir()
  file <- file.path(tmp, "B_34.pdf")
  file.create(file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(...) {
      called <<- TRUE
    },
    .package = "utils"
  )

  get_ser_pdf(ser, out_dir = tmp, overwrite = FALSE, verbose = FALSE)

  expect_false(called)
})

test_that("get_ser_pdf() downloads when overwrite = TRUE", {

  ser <- data.frame(codeser = "C56")
  tmp <- tempdir()
  file <- file.path(tmp, "C_56.pdf")
  file.create(file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      called <<- TRUE
      file.create(destfile)
    },
    .package = "utils"
  )

  get_ser_pdf(ser, out_dir = tmp, overwrite = TRUE, verbose = FALSE)

  expect_true(called)
  expect_true(file.exists(file))
})

test_that("get_ser_pdf() handles download errors gracefully", {

  ser <- data.frame(codeser = "D78")
  tmp <- tempdir()

  local_mocked_bindings(
    download.file = function(...) stop("network error"),
    .package = "utils"
  )

  expect_silent(
    get_ser_pdf(ser, out_dir = tmp, verbose = FALSE)
  )
})

test_that("get_ser_pdf() downloads each unique codeser only once", {

  ser <- data.frame(codeser = c("E12", "E12", NA))
  tmp <- tempdir()

  calls <- 0

  local_mocked_bindings(
    download.file = function(...) {
      calls <<- calls + 1
    },
    .package = "utils"
  )

  get_ser_pdf(ser, out_dir = tmp, verbose = FALSE)

  expect_equal(calls, 1)
})

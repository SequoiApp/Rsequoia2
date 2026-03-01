test_that("get_ser_pdf() validates id_ser", {

  expect_error(get_ser_pdf(NULL), "must be a non-empty vector")
  expect_error(get_ser_pdf(character(0)),"must be a non-empty vector")

})

test_that("get_ser_pdf() creates output directory if missing", {

  id_ser <- "A12"
  tmp <- tempfile()

  local_mocked_bindings(
    download.file = function(...) NULL,
    .package = "utils"
  )

  expect_false(dir.exists(tmp))

  get_ser_pdf(id_ser, dirname = tmp, verbose = FALSE)

  expect_true(dir.exists(tmp))
})

test_that("get_ser_pdf() skips download if file exists and overwrite = FALSE", {

  id_ser <- "B34"
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

  get_ser_pdf(id_ser, dirname = tmp, overwrite = FALSE, verbose = FALSE)

  expect_false(called)
})

test_that("get_ser_pdf() downloads when overwrite = TRUE", {

  id_ser <- "C56"
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

  get_ser_pdf(id_ser, dirname = tmp, overwrite = TRUE, verbose = FALSE)

  expect_true(called)
  expect_true(file.exists(file))
})

test_that("get_ser_pdf() handles download errors gracefully", {

  id_ser <- "D78"
  tmp <- tempdir()

  local_mocked_bindings(
    download.file = function(...) stop("network error"),
    .package = "utils"
  )

  expect_silent(
    get_ser_pdf(id_ser, dirname = tmp, verbose = FALSE)
  )
})

test_that("get_ser_pdf() downloads each unique codeser only once", {

  id_ser <- c("E12", "E12", "C20")
  tmp <- tempdir()

  calls <- 0

  local_mocked_bindings(
    download.file = function(...) {
      calls <<- calls + 1
    },
    .package = "utils"
  )

  get_ser_pdf(id_ser, dirname = tmp, verbose = FALSE)

  expect_equal(calls, 2)
})

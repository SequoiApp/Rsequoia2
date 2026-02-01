test_that("get_pedology_pdf() errors when id_ucs is missing", {

  pedology <- data.frame(foo = 1:3)

  expect_error(
    get_pedology_pdf(pedology, verbose = FALSE),
    "id_ucs"
  )
})

test_that("get_pedology_pdf() returns NULL when no valid id_ucs", {

  pedology <- data.frame(id_ucs = c(NA, NA))

  res <- get_pedology_pdf(pedology, verbose = FALSE)

  expect_null(res)
})

test_that("get_pedology_pdf() creates output directory if missing", {

  pedology <- data.frame(id_ucs = "A12")
  tmp <- tempfile()

  local_mocked_bindings(
    download.file = function(...) NULL,
    .package = "utils"
  )

  expect_false(dir.exists(tmp))

  get_pedology_pdf(pedology, out_dir = tmp, verbose = FALSE)

  expect_true(dir.exists(tmp))
})

test_that("get_pedology_pdf() skips download if file exists and overwrite = FALSE", {

  pedology <- data.frame(id_ucs = "B34")
  tmp <- tempdir()
  file <- file.path(tmp, "id_ucs_B34.pdf")
  file.create(file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(...) {
      called <<- TRUE
    },
    .package = "utils"
  )

  get_pedology_pdf(pedology, out_dir = tmp, overwrite = FALSE, verbose = FALSE)

  expect_false(called)
})

test_that("get_pedology_pdf() downloads when overwrite = TRUE", {

  pedology <- data.frame(id_ucs = "C56")
  tmp <- tempdir()
  file <- file.path(tmp, "id_ucs_C56.pdf")
  file.create(file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      called <<- TRUE
      file.create(destfile)
    },
    .package = "utils"
  )

  get_pedology_pdf(pedology, out_dir = tmp, overwrite = TRUE, verbose = FALSE)

  expect_true(called)
  expect_true(file.exists(file))
})

test_that("get_pedology_pdf() handles download errors gracefully", {

  pedology <- data.frame(id_ucs = "D78")
  tmp <- tempdir()

  local_mocked_bindings(
    download.file = function(...) stop("network error"),
    .package = "utils"
  )

  expect_silent(
    get_pedology_pdf(pedology, out_dir = tmp, verbose = FALSE)
  )
})

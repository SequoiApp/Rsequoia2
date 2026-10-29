test_that("get_guides_pdf() validates guides_urls", {

  expect_error(
    get_guides_pdf(NULL),
    "must be <data.frame>"
  )

  expect_error(
    get_guides_pdf(data.frame()),
    "can't be empty"
  )

})

test_that("get_guides_pdf() validates dirname", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = ""),
    "non-empty character scalar"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = NA_character_),
    "non-empty character scalar"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = c("a", "b")),
    "non-empty character scalar"
  )

})

test_that("get_guides_pdf() validates overwrite and verbose", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_error(
    get_guides_pdf(guides_urls, dirname = tmp, overwrite = 1),
    "single non-missing logical"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = tmp, overwrite = NA),
    "single non-missing logical"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = tmp, verbose = 1),
    "single non-missing logical"
  )

  expect_error(
    get_guides_pdf(guides_urls, dirname = tmp, verbose = NA),
    "single non-missing logical"
  )

})

test_that("get_guides_pdf() creates output directory if missing", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeBin(as.raw(1:3), destfile)
      0L
    },
    .package = "utils"
  )

  expect_false(dir.exists(tmp))

  result <- get_guides_pdf(
    guides_urls,
    dirname = tmp,
    verbose = FALSE
  )

  expect_true(dir.exists(tmp))
  expect_identical(result, normalizePath(tmp))
})

test_that("get_guides_pdf() skips existing files when overwrite is FALSE", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  file <- file.path(tmp, "guide.pdf")
  writeBin(as.raw(1:3), file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(...) {
      called <<- TRUE
      0L
    },
    .package = "utils"
  )

  get_guides_pdf(
    guides_urls,
    dirname = tmp,
    overwrite = FALSE,
    verbose = FALSE
  )

  expect_false(called)
  expect_true(file.exists(file))
})

test_that("get_guides_pdf() downloads when overwrite is TRUE", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  file <- file.path(tmp, "guide.pdf")
  writeBin(as.raw(1:3), file)

  called <- FALSE

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      called <<- TRUE
      writeBin(as.raw(4:6), destfile)
      0L
    },
    .package = "utils"
  )

  get_guides_pdf(
    guides_urls,
    dirname = tmp,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(called)
  expect_identical(readBin(file, "raw", n = 3L), as.raw(4:6))
})

test_that("get_guides_pdf() tries subsequent URLs after a failure", {

  guides_urls <- data.frame(
    guide_id = c(1L, 1L),
    url = c(
      "https://example.com/first.pdf",
      "https://example.com/second.pdf"
    )
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  calls <- character()

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      calls <<- c(calls, url)

      if (grepl("first.pdf", url, fixed = TRUE)) {
        stop("network error")
      }

      writeBin(as.raw(1:3), destfile)
      0L
    },
    .package = "utils"
  )

  get_guides_pdf(
    guides_urls,
    dirname = tmp,
    verbose = FALSE
  )

  expect_equal(
    calls,
    c(
      "https://example.com/first.pdf",
      "https://example.com/second.pdf"
    )
  )

  expect_true(file.exists(file.path(tmp, "second.pdf")))
})

test_that("get_guides_pdf() handles download errors gracefully", {

  guides_urls <- data.frame(
    guide_id = 1L,
    url = "https://example.com/guide.pdf"
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    download.file = function(...) {
      stop("network error")
    },
    .package = "utils"
  )

  expect_silent(
    get_guides_pdf(
      guides_urls,
      dirname = tmp,
      verbose = FALSE
    )
  )
})

test_that("get_guides_pdf() downloads each unique URL only once", {

  guides_urls <- data.frame(
    guide_id = c(1L, 1L, 1L),
    url = c(
      "https://example.com/guide.pdf",
      "https://example.com/guide.pdf",
      "https://example.com/other.pdf"
    )
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  calls <- character()

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      calls <<- c(calls, url)
      writeBin(as.raw(1:3), destfile)
      0L
    },
    .package = "utils"
  )

  get_guides_pdf(
    guides_urls,
    dirname = tmp,
    verbose = FALSE
  )

  expect_length(calls, 1L)
  expect_equal(calls, "https://example.com/guide.pdf")
})

test_that("get_guides_pdf() builds CNPF download URLs", {

  guides_urls <- data.frame(
    guide_id = 12L,
    url = "https://cloud.cnpf.fr/s/abcdef"
  )

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  downloaded_url <- NULL
  downloaded_file <- NULL

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      downloaded_url <<- url
      downloaded_file <<- destfile
      writeBin(as.raw(1:3), destfile)
      0L
    },
    .package = "utils"
  )

  get_guides_pdf(
    guides_urls,
    dirname = tmp,
    verbose = FALSE
  )

  expect_equal(
    downloaded_url,
    "https://cloud.cnpf.fr/public.php/dav/files/abcdef/"
  )

  expect_equal(
    basename(downloaded_file),
    "Guide_12.pdf"
  )

  expect_true(file.exists(downloaded_file))
})

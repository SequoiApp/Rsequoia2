test_that("seq1_read returns empty sf when no file found", {

  local_mocked_bindings(
    list.files = function(...) character(0),
    .package = "base"
  )

  expect_warning(res <- seq1_read(dirname = "dummy_dir", layer = "com_line"),
                 "No matched file for")
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
})

test_that("seq1_read aborts if layer is invalid", {

  expect_error(
    seq1_read(dirname = ".", layer = "unknown_layer"),
    "must be one of"
  )
})

test_that("seq1_read aborts if multiple files found", {

  local_mocked_bindings(
    list.files = function(...) c("file1.shp", "file2.shp"),
    .package = "base"
  )

  expect_error(
    seq1_read(dirname = "dummy_dir", layer = "com_line"),
    "Multiple files detected"
  )
})

test_that("seq1_read calls sf::read_sf with the correct file", {

  dummy_file <- "dummy.shp"

  local_mocked_bindings(
    list.files = function(...) dummy_file,
    .package = "base"
  )

  read_called <- NULL
  local_mocked_bindings(
    read_sf = function(file, ...) {
      read_called <<- file
      seq_empty
    },
    .package = "sf"
  )

  res <- seq1_read(dirname = "dummy_dir", layer = "com_line")

  expect_equal(read_called, dummy_file)
  expect_s3_class(res, "sf")
})

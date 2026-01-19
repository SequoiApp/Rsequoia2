test_that("seq1_id returns correct prefix when one file is found", {

  local_mocked_bindings(
    list.files = function(...) "PROJET_PARCA_polygon.shp",
    .package = "base"
  )

  res <- seq1_id("dummy_dir")

  expect_equal(res, "PROJET")
})

test_that("seq1_id aborts when no file found", {

  local_mocked_bindings(
    list.files = function(...) character(0),
    .package = "base"
  )

  expect_warning(
    seq1_id("dummy_dir"),
    "No _PARCA_ file detected"
  )
})

test_that("seq1_id aborts when several files found", {

  local_mocked_bindings(
    list.files = function(...) c(
      "A_PARCA_polygon.shp",
      "B_PARCA_polygon.shp"
    ),
    .package = "base"
  )

  expect_error(
    seq1_id("dummy_dir"),
    "Multiple files _PARCA_ detected"
  )
})

test_that("seq1_id works with recursive path simulation", {

  local_mocked_bindings(
    list.files = function(...) "sub/folder/TEST_PARCA_polygon.shp",
    .package = "base"
  )

  res <- seq1_id("dummy_dir")

  expect_equal(res, "TEST")
})

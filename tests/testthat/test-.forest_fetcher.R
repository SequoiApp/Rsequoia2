test_that(".forest_fetcher() returns null when no data", {

  testthat::local_mocked_bindings(
    get_forest = function(...) NULL
  )

  path <- .forest_fetcher(
    x = Rsequoia2:::seq_poly,
    dirname = tempdir(),
    verbose = FALSE
  )

  expect_null(path)

})

test_that(".forest_fetcher() writes forest data", {

  testthat::local_mocked_bindings(
    get_forest = function(...) Rsequoia2:::seq_poly,
    seq_write = function(...) "forest.gpkg"
  )

  path <- .forest_fetcher(
    x = Rsequoia2:::seq_poly,
    dirname = tempdir(),
    verbose = FALSE
  )

  expect_equal(path, "forest.gpkg")

})

test_that(".forest_fetcher() adds project identifier", {

  forest <- Rsequoia2:::seq_poly
  identifier <- seq_field("identifier")$name

  testthat::local_mocked_bindings(
    get_forest = function(...) forest,
    seq_field = function(...) list(name = identifier),
    seq_write = function(x, ...) {
      expect_equal(
        x[[identifier]],
        rep("PROJECT01", nrow(x))
      )
      "forest.gpkg"
    }
  )

  path <- .forest_fetcher(
    x = Rsequoia2:::seq_poly,
    dirname = tempdir(),
    id = "PROJECT01",
    verbose = FALSE
  )

  expect_equal(path, "forest.gpkg")

})

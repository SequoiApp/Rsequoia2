
test_that("get_com_point() returns NULL when no commune is found", {

  testthat::local_mocked_bindings(
    get_com_poly = function(...) NULL
  )

  expect_null(get_com_point(Rsequoia2:::seq_poly, verbose = FALSE))
})

test_that("get_com_point() returns sf when commune is found", {

  testthat::local_mocked_bindings(
    get_com_poly = function(...) Rsequoia2:::seq_poly
  )

  com <- get_com_point(Rsequoia2:::seq_poly, verbose = FALSE)
  expect_s3_class(com, "sf")

})


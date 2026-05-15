
test_that("get_com_line() returns NULL when no commune is found", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_null(get_com_line(Rsequoia2:::seq_line, verbose = FALSE))
})

test_that("get_com_line() returns sf when commune is found", {

  testthat::local_mocked_bindings(
    get_com_poly = function(...) Rsequoia2:::seq_poly
  )

  com <- get_com_line(Rsequoia2:::seq_poly, verbose = FALSE)
  expect_s3_class(com, "sf")

})


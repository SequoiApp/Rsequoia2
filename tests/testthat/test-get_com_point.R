
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

test_that("get_com_point() returns pointstring sf with correct crs", {

  testthat::local_mocked_bindings(
    get_com_poly = function(...) Rsequoia2:::seq_poly
  )

  com <- get_com_point(Rsequoia2:::seq_poly, verbose = FALSE)
  expect_s3_class(com, "sf")
  expect_equal(sf::st_crs(com), sf::st_crs(2154))
  expect_all_true(sf::st_geometry_type(com) %in% c("POINT", "MULTIPOINT"))
})

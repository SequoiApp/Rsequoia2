
test_that("get_com_poly() returns NULL when no commune is found", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  expect_null(get_com_poly(Rsequoia2:::seq_poly, verbose = FALSE))
})

test_that("get_com_poly() returns sf when commune is found", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  com <- get_com_poly(Rsequoia2:::seq_poly, verbose = FALSE)
  expect_s3_class(com, "sf")

})

test_that("get_com_poly() returns sf with correct crs", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly |> sf::st_transform(4326),
    .package = "happign"
  )

  com <- get_com_poly(Rsequoia2:::seq_poly, verbose = FALSE)
  expect_s3_class(com, "sf")
  expect_equal(sf::st_crs(com), sf::st_crs(2154))
})

test_that("get_com_poly() is properly normalize", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  com <- get_com_poly(Rsequoia2:::seq_poly, verbose = FALSE)

  col_name <- lapply(seq_table("com_poly"), \(x) seq_field(x)$name)
  expect_all_true(col_name %in% names(com))

})

test_that("get_com_poly() verbose mode", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  expect_message(
    get_com_poly(Rsequoia2:::seq_poly, verbose = TRUE),
    "Downloading communes dataset"
  )

})

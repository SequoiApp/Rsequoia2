test_that("get_accessibility() validates x", {

  expect_error(get_accessibility(42), "should be of class")
  expect_error(get_accessibility("a"), "should be of class")

})

test_that("get_accessibility() validates type length", {

  expect_error(
    get_accessibility(Rsequoia2:::seq_poly, type = c("porteur", "skidder")),
    "must contain exactly one element"
  )

})

test_that("get_accessibility() validates type value", {

  expect_error(
    get_accessibility(Rsequoia2:::seq_poly, type = "invalid"),
    "should be one of"
  )

})

test_that("get_accessibility() returns NULL when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  res <- get_accessibility(Rsequoia2:::seq_poly, type = "porteur", verbose = FALSE)

  expect_null(res)

})

test_that("get_accessibility() returns sf when data exists", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_accessibility(Rsequoia2:::seq_poly, type = "porteur", verbose = FALSE)

  expect_s3_class(res, "sf")

  expect_gt(nrow(res), 0)

})

test_that("get_accessibility() returns EPSG:2154", {

  x <- sf::st_transform(Rsequoia2:::seq_poly, 4326)

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_accessibility(x, type = "porteur", verbose = FALSE)

  expect_equal(sf::st_crs(res)$epsg, 2154)

})

test_that("get_accessibility() uses correct layer", {

  tracker <- list(layer = NULL)
  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      tracker$layer <<- layer
      Rsequoia2:::seq_poly
    },
    .package = "happign"
  )

  get_accessibility(Rsequoia2:::seq_poly, type = "porteur", verbose = FALSE)

  expect_equal(
    tracker$layer,
    "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_porteur"
  )

})

test_that("get_accessibility() uses correct layer", {

  tracker <- list(layer = NULL)
  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      tracker$layer <<- layer
      Rsequoia2:::seq_poly
    },
    .package = "happign"
  )

  get_accessibility(Rsequoia2:::seq_poly, type = "skidder", verbose = FALSE)

  expect_equal(
    tracker$layer,
    "IGNF_ACCESSIBILITE-PHYSIQUE-FORETS-:acces_skidder"
  )

})

test_that("get_accessibility() prints message when verbose", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  expect_message(
    get_accessibility(Rsequoia2:::seq_poly, type = "porteur", verbose = TRUE),
    "Downloading forest"
  )

})

test_that("get_accessibility() is silent when verbose = FALSE", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  expect_no_message(
    get_accessibility(Rsequoia2:::seq_poly, type = "porteur", verbose = FALSE)
  )

})

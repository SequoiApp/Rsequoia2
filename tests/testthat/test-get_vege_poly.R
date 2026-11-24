test_that("get_vege_poly() validates x", {
  expect_error(get_vege_poly(42), "x.*sf")
  expect_error(get_vege_poly("a"), "x.*sf")
})

test_that("get_vege_poly() returns empty sf when WFS returns NULL", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) NULL,
    .package = "happign"
  )

  res <- get_vege_poly(Rsequoia2:::seq_point) |> suppressMessages()

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 0)
  expect_true(all(st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() returns polygons when WFS returns data", {

  local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_vege_poly(Rsequoia2:::seq_poly, buffer = 0, clip = FALSE, tol = 0)

  expect_s3_class(res, "sf")
  expect_gt(nrow(res), 0)
  expect_true(all(sf::st_geometry_type(res) == "POLYGON"))
})

test_that("get_vege_poly() sets type and source fields", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_poly,
    .package = "happign"
  )

  res <- get_vege_poly(Rsequoia2:::seq_poly, 0)

  type <- seq_field("type")$name
  source <- seq_field("source")$name

  expect_true(type %in% names(res))
  expect_true(source %in% names(res))

  expect_true(all(res[[type]] == "FOR"))
  expect_true(all(res[[source]] == "ignf_masque_foret"))
})

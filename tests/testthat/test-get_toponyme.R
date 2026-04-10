test_that("get_toponyme() returns null when no data", {

  testthat::local_mocked_bindings(
    get_wfs = function(...) Rsequoia2:::seq_empty,
    .package = "happign"
  )

  toponyme <- get_toponyme(Rsequoia2:::seq_poly, verbose = FALSE)

  # tests
  expect_null(toponyme, "sf")
})

test_that("get_toponyme() works", {

  x <- Rsequoia2:::seq_poly

  pt <- Rsequoia2:::seq_point
  pt$cleabs_de_l_objet <- c("PAIHYDRO", "CIMETIER", "FORETPUB")
  pt$nature_de_l_objet <- c("Rivière", "Cimetiere", "Bois")
  pt$graphie_du_toponyme <- c("Seine", "Cimetiere Nord", "Foret X")

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  toponyme <- get_toponyme(x, verbose = FALSE)

  expect_s3_class(toponyme, "sf")

  type <- seq_field("type")$name
  name <- seq_field("name")$name
  nature <- seq_field("nature")$name
  source <- seq_field("source")$name

  expect_true(all(type %in% names(toponyme)))
  expect_true(all(name %in% names(toponyme)))
  expect_true(all(nature %in% names(toponyme)))
  expect_true(all(source %in% names(toponyme)))

  expect_true(any(toponyme[[type]] == "HYD"))
  expect_true(any(toponyme[[type]] == "CIM"))
  expect_true(any(toponyme[[type]] == "VEG"))
})

test_that("get_toponyme() force crs to 2154", {

  x <- Rsequoia2:::seq_poly

  pt <- Rsequoia2:::seq_point
  pt$cleabs_de_l_objet <- c("PAIHYDRO", "CIMETIER", "FORETPUB")
  pt$nature_de_l_objet <- c("Rivière", "Cimetiere", "Bois")
  pt$graphie_du_toponyme <- c("Seine", "Cimetiere Nord", "Foret X")

  testthat::local_mocked_bindings(
    get_wfs = function(...) pt,
    .package = "happign"
  )

  toponyme <- get_toponyme(x, verbose = FALSE)

  expect_s3_class(toponyme, "sf")
  expect_equal(sf::st_crs(toponyme)$srid, "EPSG:2154")
})

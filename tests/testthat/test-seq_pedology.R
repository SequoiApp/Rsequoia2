test_that("seq_pedology() returns a valid file path", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  openxlsx2::write_xlsx(m, file.path(seq_cache, "TEST_matrice.xlsx"))

  # parca
  seq_parca(seq_cache, verbose = FALSE)

  # seq_pedology
  path <- seq_pedology(seq_cache, verbose = FALSE)

  expect_type(path, "character")
  expect_true(file.exists(path))
})

test_that("seq_pedology() writes valid pedology layer when features exist", {
  skip_on_cran()
  skip_on_ci()

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  openxlsx2::write_xlsx(m, file.path(seq_cache, "TEST_matrice.xlsx"))

  seq_parca(seq_cache, verbose = FALSE)

  path <- seq_pedology(seq_cache, verbose = FALSE)
  pedo <- sf::read_sf(path)

  expect_s3_class(pedo, "sf")
  expect_true(sf::st_crs(pedo)$epsg == 2154)
})

test_that("seq_pedology() returns NULL and writes nothing when no features exist", {

  skip_on_cran()
  skip_on_ci()

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # fake matrice
  m <- fake_matrice(id = "TEST")
  openxlsx2::write_xlsx(m, file.path(seq_cache, "TEST_matrice.xlsx"))

  # fake parca (no pedology intersection)
  p <- fake_parca()
  seq_write(p, "parca", dirname = seq_cache)

  # run
  path <- seq_pedology(seq_cache, verbose = FALSE)

  # expectations
  expect_null(path)

  pedo_path <- get_path("v.sol.pedo.poly", dirname = seq_cache)
  expect_false(file.exists(pedo_path))
})

test_that("seq_curves() writes valid toponymy layer", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # seq_curves
  line <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(matrix(1:10, , 2)), crs = 2154))

  testthat::local_mocked_bindings(
    get_curves = function(...) line
  )

  path <- seq_curves(seq_cache, verbose = FALSE)

  # tests
  expect_true(nzchar(path))
  curves <- sf::read_sf(path)
  expect_s3_class(curves, "sf")
})

test_that("seq_curves() returns NULL when no toponymic features exist", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  # seq_curves
  testthat::local_mocked_bindings(
    get_curves = function(...) NULL
  )

  expect_warning(path <- seq_curves(seq_cache, verbose = FALSE),
                 "No hypsometric")
  expect_null(path)
})

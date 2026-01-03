test_that("seq_prsf() writes valid PRSF point layer", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # seq_hydro
  path <- seq_prsf(seq_cache, verbose = FALSE)

  # tests
  expect_true(nzchar(path))
  curves <- sf::read_sf(path)
  expect_true(sf::st_crs(curves)$epsg == 2154)
  expect_true(all(sf::st_geometry_type(curves) == "POINT"))
  expect_true(nrow(curves) > 0)
  expect_s3_class(curves, "sf")
})

test_that("seq_prsf() returns NULL when no PRSF features exist", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  # seq_hydro
  path <- seq_prsf(seq_cache, verbose = FALSE)

  expect_null(path)
})

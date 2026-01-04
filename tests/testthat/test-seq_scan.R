test_that("seq_scan() downloads and writes scan rasters", {
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

  # seq_scan
  paths <- seq_scan(dirname = seq_cache,
                    type = "scan25",
                    buffer = 10,
                    res = 0.8,
                    overwrite = TRUE,
                    verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_named(paths, "scan25")
  expect_true(file.exists(paths$scan25))
})

test_that("seq_scan() returns NULL when no rasters exist", {
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

  # seq_scan
  paths <- seq_scan(dirname = seq_cache,
                    type = "scan25",
                    buffer = 10,
                    res = 0.8,
                    overwrite = TRUE,
                    verbose = FALSE)

  expect_null(paths)
})

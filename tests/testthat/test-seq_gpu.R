test_that("seq_gpu() writes valid GPU layers", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "53085", section = "0B", numero = "0081")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # seq_gpu
  paths <- seq_gpu(seq_cache, verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_true(length(paths) > 0)
  expect_true(all(vapply(paths, nzchar, logical(1))))

  # check each written layer
  for (path in paths) {
    f <- sf::read_sf(path)

    expect_s3_class(f, "sf")
    expect_true(sf::st_crs(f)$epsg == 2154)
    expect_true(nrow(f) > 0)

    # geometry must be one of GPU-supported types
    expect_true(
      all(sf::st_geometry_type(f) %in% c(
        "POLYGON", "MULTIPOLYGON",
        "LINESTRING", "MULTILINESTRING",
        "POINT", "MULTIPOINT"
      ))
    )
  }
})

test_that("seq_gpu() returns empty list when no GPU features exist", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice minimaliste
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # fake parca hors GPU coverage
  p <- fake_parca()
  seq_write(p, "parca", dirname = seq_cache)

  # seq_gpu
  paths <- seq_gpu(seq_cache, verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_length(paths, 0)
})

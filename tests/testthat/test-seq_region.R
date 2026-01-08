test_that("seq_region() writes valid polygon layer", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0B", numero = "0081")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # call seq_region
  paths <- seq_region(
    dirname = seq_cache,
    types = c("ser", "rfn"),
    verbose = FALSE,
    overwrite = TRUE
  )

  # tests
  expect_type(paths, "list")
  expect_named(paths, c("ser", "rfn"))

  for (p in paths) {
    expect_true(file.exists(p))
    region <- sf::read_sf(p)
    expect_s3_class(region, "sf")
    expect_true(nrow(region) > 0)
    expect_true(sf::st_crs(region)$epsg == 2154)
    expect_true(all(sf::st_geometry_type(region) %in% c("POLYGON", "MULTIPOLYGON")))
  }
})

test_that("seq_region() returns NULL when no features exist", {
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

  # seq_region
  path <- seq_region(seq_cache, verbose = FALSE)

  expect_null(path)
})

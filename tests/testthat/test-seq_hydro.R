test_that("seq_hydro() returns a named list of three paths", {
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
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  # seq_hydro
  paths <- seq_hydro(seq_cache, verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_length(paths, 3)

  expect_true(all(nzchar(unlist(paths))))
  expect_true(all(file.exists(unlist(paths))))
})

test_that("hydro layers have correct geometry types and CRS", {
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
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  # seq_hydro
  paths <- seq_hydro(seq_cache, verbose = FALSE)

  # tests
  poly  <- sf::read_sf(paths[[1]])
  line  <- sf::read_sf(paths[[2]])
  point <- sf::read_sf(paths[[3]])

  expect_true(sf::st_crs(poly)$epsg == 2154)
  expect_true(sf::st_crs(line)$epsg == 2154)
  expect_true(sf::st_crs(point)$epsg == 2154)

  expect_true(all(sf::st_geometry_type(poly)  %in% c("POLYGON", "MULTIPOLYGON")))
  expect_true(all(sf::st_geometry_type(line)  == "LINESTRING"))
  expect_true(all(sf::st_geometry_type(point) == "POINT"))
})

test_that("seq_hydro() writes valid layers when features exist", {
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
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  # seq_hydro
  paths <- seq_hydro(seq_cache, verbose = FALSE)

  # tests
  poly  <- sf::read_sf(paths[[1]])
  line  <- sf::read_sf(paths[[2]])
  point <- sf::read_sf(paths[[3]])

  expect_true(nrow(poly) > 0)
  expect_true(nrow(line) > 0)
  expect_true(nrow(point) > 0)

  expect_s3_class(poly, "sf")
  expect_s3_class(line, "sf")
  expect_s3_class(point, "sf")
})

test_that("seq_hydro() writes valid empty layers when no features exist", {
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
  paths <- seq_hydro(seq_cache, verbose = FALSE)

  # tests
  poly  <- sf::read_sf(paths[[1]])
  line  <- sf::read_sf(paths[[2]])
  point <- sf::read_sf(paths[[3]])

  expect_equal(nrow(poly), 0)
  expect_equal(nrow(line), 0)
  expect_equal(nrow(point), 0)

  expect_s3_class(poly, "sf")
  expect_s3_class(line, "sf")
  expect_s3_class(point, "sf")
})

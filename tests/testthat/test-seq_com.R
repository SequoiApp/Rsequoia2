test_that("seq_com() returns a named list of five paths", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq_com")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # seq_com
  paths <- seq_com(seq_cache, verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_length(paths, 5)

  expect_true(all(nzchar(unlist(paths))))
  expect_true(all(file.exists(unlist(paths))))
})

test_that("seq_com() layers have correct geometry types and CRS", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq_com")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE, overwrite = TRUE)

  # seq_com
  paths <- seq_com(seq_cache, verbose = FALSE, overwrite = TRUE)

  # read layers
  topo_poly     <- sf::read_sf(paths[[1]])
  topo_line     <- sf::read_sf(paths[[2]])
  topo_point    <- sf::read_sf(paths[[3]])
  graphic_line  <- sf::read_sf(paths[[4]])
  graphic_point <- sf::read_sf(paths[[5]])

  # CRS
  expect_true(sf::st_crs(topo_poly)$epsg == 2154)
  expect_true(sf::st_crs(topo_line)$epsg == 2154)
  expect_true(sf::st_crs(topo_point)$epsg == 2154)
  expect_true(sf::st_crs(graphic_line)$epsg == 2154)
  expect_true(sf::st_crs(graphic_point)$epsg == 2154)

  # geometry types
  expect_true(all(sf::st_geometry_type(topo_poly) %in% c("POLYGON", "MULTIPOLYGON")))
  expect_true(all(sf::st_geometry_type(topo_line) == "MULTILINESTRING"))
  expect_true(all(sf::st_geometry_type(topo_point) == "POINT"))
  expect_true(all(sf::st_geometry_type(graphic_line) == "MULTILINESTRING"))
  expect_true(all(sf::st_geometry_type(graphic_point) == "POINT"))
})

test_that("seq_com() does not write layers when no commune intersects", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq_com")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice factice
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # fake parca hors France
  p <- fake_parca() |> sf::st_buffer(5)
  seq_write(p, "parca", dirname = seq_cache, overwrite = TRUE)

  # seq_com
  paths <- seq_com(seq_cache, verbose = FALSE)

  # tests
  expect_type(paths, "list")
  expect_length(paths, 0)

  # aucun fichier écrit
  expect_true(all(vapply(paths, function(x) is.null(x) || !file.exists(x), logical(1))))
})

test_that("graphic commune layers are spatially smaller than topo layers", {
  skip_on_cran()
  skip_on_ci()

  # cache dir
  seq_cache <- file.path(tempdir(), "seq_com")
  dir.create(seq_cache, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  # matrice
  m <- fake_matrice(insee = "71279", section = "0C", numero = "0380")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  # parca
  p <- seq_parca(seq_cache, verbose = FALSE)

  # seq_com
  paths <- seq_com(seq_cache, verbose = FALSE)

  topo_line     <- sf::read_sf(paths[[2]])
  graphic_line  <- sf::read_sf(paths[[4]])

  expect_lt(
    sum(sf::st_length(graphic_line)),
    sum(sf::st_length(topo_line))
  )
})

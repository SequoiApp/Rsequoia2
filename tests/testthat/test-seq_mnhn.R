test_that("seq_mnhn() works with mocked get_mnhn()", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  local_mocked_bindings(
    get_mnhn = function(x, key, buffer, overwrite, verbose){
      x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
    }
  )

  mnhn_path <- seq_mnhn(dirname = seq_cache, buffer = 500, key = c("pn", "pnr"), verbose = FALSE)
  pn_path <- mnhn_path[[1]]
  pnr_path <- mnhn_path[[2]]
  expect_all_true(c(file.exists(pn_path), file.exists(pnr_path)))

  pn <- sf::read_sf(pn_path)
  expect_s3_class(pn, "sf")

  pnr <- sf::read_sf(pnr_path)
  expect_s3_class(pnr, "sf")

})


test_that("seq_mnhn() works when verbose = TRUE", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = seq_cache)

  local_mocked_bindings(
    get_mnhn = function(x, key, buffer, overwrite, verbose){
      x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
    }
  )

  expect_message(
    mnhn_path <- seq_mnhn(dirname = seq_cache, buffer = 500, key = c("pn", "pnr"), verbose = TRUE),
    "non-empty layers found:"
  )

  pn_path <- mnhn_path[[1]]
  pnr_path <- mnhn_path[[2]]
  expect_all_true(c(file.exists(pn_path), file.exists(pnr_path)))

  pn <- sf::read_sf(pn_path)
  expect_s3_class(pn, "sf")

  pnr <- sf::read_sf(pnr_path)
  expect_s3_class(pnr, "sf")

})

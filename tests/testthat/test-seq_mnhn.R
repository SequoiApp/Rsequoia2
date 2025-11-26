test_that("seq_mnhn() works with mocked get_parca()", {

  d <- tempdir()

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = d)

  local_mocked_bindings(
    get_mnhn = function(x, key, buffer, overwrite, verbose){
      x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
    }
  )

  mnhn_path <- seq_mnhn(dirname = d, buffer = 500, key = c("pn", "pnr"), verbose = FALSE)
  pn_path <- mnhn_path[[1]]
  pnr_path <- mnhn_path[[2]]
  expect_all_true(c(file.exists(pn_path), file.exists(pnr_path)))

  pn <- sf::read_sf(pn_path)
  expect_s3_class(pn, "sf")

  pnr <- sf::read_sf(pnr_path)
  expect_s3_class(pnr, "sf")

  on.exit(unlink(c(m_path, parca_path, pn_path, pnr_path)))
})


test_that("seq_mnhn() works when verbose = TRUE", {

  d <- tempdir()

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = d)

  local_mocked_bindings(
    get_mnhn = function(x, key, buffer, overwrite, verbose){
      x <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154))
    }
  )

  expect_message(
    mnhn_path <- seq_mnhn(dirname = d, buffer = 500, key = c("pn", "pnr"), verbose = TRUE),
    "non-empty layers found:"
  )

  pn_path <- mnhn_path[[1]]
  pnr_path <- mnhn_path[[2]]
  expect_all_true(c(file.exists(pn_path), file.exists(pnr_path)))

  pn <- sf::read_sf(pn_path)
  expect_s3_class(pn, "sf")

  pnr <- sf::read_sf(pnr_path)
  expect_s3_class(pnr, "sf")

  on.exit(unlink(c(m_path, parca_path, pn_path, pnr_path)))
})

test_that("seq_parca() works with mocked get_parca()", {

  d <- tempdir()
  m <- fake_matrice(
    id = "TEST", lieu_dit = c(NA, "OWNER"), numero = c("0001", "0002")
  )
  m_path <- file.path(d, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  local_mocked_bindings(
    get_parca = function(idu, bdp_geom, lieu_dit, verbose){
      fake_raw_parca(
        numero =c("0001", "0002"),
        lieu_dit = c("LIEU_DIT1", "LIEU_DIT2")
      )
    }
  )

  parca_path <- seq_parca(dirname = d, verbose = F)
  expect_true(file.exists(parca_path))

  parca <- sf::read_sf(parca_path)

  expect_s3_class(parca, "sf")
  expect_equal(parca$LIEU_DIT, c("LIEU_DIT1", "OWNER")) # replace NA lieu-dit
  expect_all_equal(parca$OCCUP_SOL, "BOISEE")    # since TX_BOISEE = 0.8

  on.exit(unlink(c(m_path, parca_path)))
})


test_that("seq_parca() works with mocked get_parca()", {

  seq_dir <- file.path(tempdir(), "seq")
  dir.create(seq_dir, showWarnings = FALSE)
  on.exit(unlink(seq_dir, recursive = TRUE, force = TRUE))

  m <- fake_matrice(id = "TEST", lieu_dit = c(NA, "OWNER"), numero = c("0001", "0002"))
  m_path <- file.path(seq_dir, "TEST_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  local_mocked_bindings(
    get_parca = function(idu, bdp_geom, lieu_dit, verbose){
      fake_raw_parca(
        numero = c("0001", "0002"),
        lieu_dit = c("LIEU_DIT1", "LIEU_DIT2")
      )
    }
  )

  parca_path <- seq_parca(dirname = seq_dir, verbose = F)
  expect_true(file.exists(parca_path))

  parca <- sf::read_sf(parca_path)

  lieu_dit <- seq_field("lieu_dit")$name

  expect_s3_class(parca, "sf")
  expect_equal(parca[[lieu_dit]], c("LIEU_DIT1", "OWNER"))

})


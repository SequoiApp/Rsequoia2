test_that("seq_parca() works with mocked get_parca()", {

  d <- tempdir()
  idu <- "33103000AB0060" # Tiny commune: 33103 (Castelmoron-d'Albret)
  m_path <- create_matrice(d, id = "GET_PARCA", verbose = F)

  # Mocking get_parca
  fake_parca <- sf::st_sf(
    IDU = idu,
    REG_NOM = "TEST",
    REG_NUM = "01",
    DEP_NOM = "TEST",
    DEP_NUM = "33",
    COM_NOM = "TEST",
    COM_NUM = "33103",
    PREFIXE = "000",
    SECTION = "AB",
    NUMERO = "0060",
    LIEU_DIT = "MOCK",
    CONTENANCE = 50,
    geometry = sf::st_sfc(
      sf::st_point(c(-0.0116, 44.6794)),
      crs = 4326
    )
  )
  local_mocked_bindings(
    get_parca = function(idu, bdp_geom, lieu_dit, verbose) fake_parca
  )

  parca <- seq_parca(dirname = d, verbose = F)
  parca_path <- get_path("v.seq.parca.poly", d)

  expect_s3_class(parca, "sf")
  expect_equal(parca$IDU, idu)
  expect_equal(parca$LIEU_DIT, "NAME OF LIEU DIT")  # LIEU_DIT.x was NA → override by get_parca()
  expect_equal(parca$OCCUP_SOL, "BOISEE")    # since TX_BOISEE = 0.8
  expect_true("geometry" %in% names(parca))

  on.exit(unlink(c(m_path, parca_path)))
})


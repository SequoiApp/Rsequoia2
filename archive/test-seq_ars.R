fake_geom <- \(type) {
  geom <- sf::st_sfc(sf::st_point(c(5, 5)), crs = 2154)
  sf::st_as_sf(data.frame(type = type), geometry = geom)
}

test_that("seq_ars() works with mocked get_ars()", {

  d <- tempdir()

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca()
  parca_path <- seq_write(p, "parca", dirname = d)

  local_mocked_bindings(
    get_ars = function(x, key, buffer){
      fake_geom(key)
    }
  )

  ars_path <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppi", "ppe"), verbose = FALSE)
  expect_named(ars_path, c("v.ars.captage.point","v.ars.perimetre.poly"))
  captage_path <- ars_path[[1]]
  perimetre_path <- ars_path[[2]]
  expect_all_true(c(file.exists(captage_path), file.exists(perimetre_path)))


  captage <- sf::read_sf(captage_path)
  expect_s3_class(captage, "sf")
  expect_shape(captage, nrow = 1)
  expect_equal(captage$type, "captage")

  perimetre <- sf::read_sf(perimetre_path)
  expect_s3_class(perimetre, "sf")
  expect_shape(perimetre, nrow = 2)
  expect_equal(perimetre$type, c("ppi", "ppe"))

  on.exit(unlink(c(m_path, parca_path, captage_path, perimetre_path)))
})

test_that("seq_ars() returns NULL and warns when all layers are empty", {

  d <- tempdir()

  # fake matrice + parca
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)
  parca_path <- seq_write(fake_parca(), "parca", dirname = d)

  # mock get_ars() -> always returns NULL
  local_mocked_bindings(
    get_ars = function(x, key, buffer) NULL
  )

  expect_warning(
    res <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppi", "ppe"), verbose = FALSE),
    "All ARS layers are empty"
  )

  expect_null(res)

  on.exit(unlink(c(m_path, parca_path)))
})

test_that("seq_ars() prints messages when verbose = TRUE", {

  d <- tempdir()

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)
  parca_path <- seq_write(fake_parca(), "parca", dirname = d)

  # always return a valid layer
  local_mocked_bindings(
    get_ars = function(x, key, buffer) fake_geom(key)
  )

  expect_message(
    ars_path <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppe"), verbose = TRUE),
    "captage"
  ) |> suppressMessages()
  unlink(c(ars_path[[1]], ars_path[[2]]))

  expect_message(
    ars_path <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppe"), verbose = TRUE),
    "perimetre"
  ) |> suppressMessages()
  unlink(c(ars_path[[1]], ars_path[[2]]))


  on.exit(unlink(c(m_path, parca_path, ars_path)))
})

test_that("seq_ars() handles case where only captage has data", {

  d <- tempdir()
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)
  parca_path <- seq_write(fake_parca(), "parca", dirname = d)

  # mock: captage -> sf, ppi/ppe/ppr -> NULL
  local_mocked_bindings(
    get_ars = function(x, key, buffer) {
      if (key == "captage") fake_geom(key) else NULL
    }
  )

  ars_path <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppi", "ppe"), verbose = FALSE)

  # only captage_path should exist
  expect_true(file.exists(ars_path[[1]]))
  expect_length(ars_path, 1)
  expect_match(names(ars_path), "captage")
  expect_no_match(names(ars_path), "perimetre")

  on.exit(unlink(c(m_path, parca_path, ars_path)))

})

test_that("seq_ars() handles case where only perimetre has data", {

  d <- tempdir()
  m <- fake_matrice(id = "TEST")
  m_path <- file.path(d, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)
  parca_path <- seq_write(fake_parca(), "parca", dirname = d)

  # mock: captage -> sf, ppi/ppe/ppr -> NULL
  local_mocked_bindings(
    get_ars = function(x, key, buffer) {
      if (key != "captage") fake_geom(key) else NULL
    }
  )

  ars_path <- seq_ars(dirname = d, buffer = 10, key = c("captage", "ppi", "ppe"), verbose = FALSE)

  # only captage_path should exist
  expect_true(file.exists(ars_path[[1]]))
  expect_length(ars_path, 1)
  expect_match(names(ars_path), "perimetre")
  expect_no_match(names(ars_path), "captage")

  on.exit(unlink(c(m_path, parca_path, ars_path)))

})

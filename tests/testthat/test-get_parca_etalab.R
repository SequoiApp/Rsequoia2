test_that("get_parca_etalab() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()

  idu <- "33103000AB0060" # Tinyest commune in france: 33103
  out <- get_parca_etalab(idu)

  expect_snapshot(str(sf::st_drop_geometry(out)))

})

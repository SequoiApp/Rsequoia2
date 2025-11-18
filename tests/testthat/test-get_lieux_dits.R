test_that("get_lieux_dits() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()

  idu <- "33103000AB0060" # Tinyest commune in france: 33103
  out <- get_lieux_dits(idu)

  expect_snapshot(str(sf::st_drop_geometry(out)))

})

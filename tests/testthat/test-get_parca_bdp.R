test_that("get_parca_bdp() works with real API (local only)", {

  skip_on_cran()
  skip_on_ci()

  idu <- "29158000AZ0086"
  out <- get_parca_bdp(idu)

  expect_snapshot(str(sf::st_drop_geometry(out)))

})

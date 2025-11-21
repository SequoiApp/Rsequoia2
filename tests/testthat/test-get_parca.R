test_that("get_parca() works (local only)", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  idu <- "33103000AB0060"  # Tiny commune: 33103 (Castelmoron-d'Albret)

  out <- get_parca(idu, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

  out <- get_parca(idu, lieu_dit = TRUE, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

  out <- get_parca(idu, bdp_geom = FALSE, verbose = FALSE)
  expect_s3_class(out, "sf")
  expect_snapshot(str(sf::st_drop_geometry(out)))

})


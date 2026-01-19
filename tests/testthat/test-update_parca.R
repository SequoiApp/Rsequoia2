test_that("update_parca sets identifier and insee correctly", {
  id_field <- seq_field("identifier")$name
  dep_code_field <- seq_field("dep_code")$name
  com_code_field <- seq_field("com_code")$name
  insee_field <- seq_field("insee")$name

  parca <- Rsequoia2:::seq_poly |>
    transform(dep = "72",
              com = "187")

  res <- update_parca(parca, id = "TEST")

  expect_s3_class(res, "sf")
  expect_all_true(c(id_field, dep_code_field, com_code_field, insee_field) %in% names(res))
  expect_equal(unique(res$IDENTIFIANT), "TEST")
  expect_equal(unique(res$INSEE), "72187")
  expect_equal(
    sf::st_geometry(res),
    sf::st_geometry(parca)
  )
})

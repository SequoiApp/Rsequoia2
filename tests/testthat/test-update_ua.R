test_that("update_ua sets identifier and insee correctly", {
  id_field <- seq_field("identifier")$name
  dep_code_field <- seq_field("dep_code")$name
  com_code_field <- seq_field("com_code")$name
  insee_field <- seq_field("insee")$name
  is_wooded <- seq_field("is_wooded")$name

  ua <- Rsequoia2:::seq_poly |>
    transform(dep = "72",
              com = "187",
              OCCUP_SOL = c("BOISEE", "BOISEE", "NON BOISEE"))

  res <- update_ua(ua, id = "TEST")

  expect_s3_class(res, "sf")
  expect_all_true(c(id_field, dep_code_field, com_code_field, insee_field, is_wooded) %in% names(res))
  expect_equal(unique(res$IDENTIFIANT), "TEST")
  expect_equal(unique(res$INSEE), "72187")
  expect_equal(res$BOISE, c(TRUE, TRUE, FALSE))
  expect_equal(
    sf::st_geometry(res),
    sf::st_geometry(ua)
  )
})

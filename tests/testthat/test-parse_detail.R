test_that("parse_detail() works on real life examples", {

  l0 <- "221A J T 04 Terres 1 25 90 75,88 C TA 22,76 30"
  expect_equal(
    parse_detail(l0),
    data.frame(
      s_par = "221A", gr = "T", classe = "04", nature = "Terres",
      contenance = 1.2590, type = "detail", an = NA, prefix = NA, section = NA,
      numero = NA, lieu_dit = NA, rivoli = NA, fp = NA
    )
  )

  l1 <- "221A K T 05 Terres 1 25 90 45,53 C TA 13,66 30"
  expect_equal(
    parse_detail(l1),
    data.frame(
      s_par = "221A", gr = "T", classe = "05", nature = "Terres",
      contenance = 1.2590, type = "detail", an = NA, prefix = NA, section = NA,
      numero = NA, lieu_dit = NA, rivoli = NA, fp = NA
    )
  )

  l3 <- "221A AK T 05 Terres 2 39 40 86,58 C TA 25,97 30"
  expect_equal(
    parse_detail(l3),
    data.frame(
      s_par = "221A", gr = "T", classe = "05", nature = "Terres",
      contenance = 2.3940, type = "detail", an = NA, prefix = NA, section = NA,
      numero = NA, lieu_dit = NA, rivoli = NA, fp = NA
    )
  )

  l4 <- "260A A AG 02 Terrains d'agrement 30 33 49,85"
  expect_equal(
    parse_detail(l4),
    data.frame(
      s_par = "260A", gr = "AG", classe = "02", nature = "Terrains d'agrement",
      contenance = 0.3033, type = "detail", an = NA, prefix = NA, section = NA,
      numero = NA, lieu_dit = NA, rivoli = NA, fp = NA
    )
  )
})

test_that("parse_detail() always returns a 1-row data.frame with expected columns", {
  res <- parse_detail("221A  J  T 04 Terres 1 25 90 75,88")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)

  expect_named(
    res,
    c(
      "s_par","gr","classe","nature","contenance","type",
      "an","prefix","section","numero","lieu_dit","rivoli","fp"
    )
  )
})

test_that("parse_detail() column types are stable", {
  res <- parse_detail("221A  J  T 04 Terres 1 25 90 75,88")

  expect_type(res$s_par, "character")
  expect_type(res$gr, "character")
  expect_type(res$classe, "character")
  expect_type(res$nature, "character")
  expect_type(res$contenance, "double")
  expect_type(res$type, "character")

  expect_true(all(is.na(res$an)))
})


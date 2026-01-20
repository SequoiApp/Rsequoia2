fake_data <- function(dep) {
  data.frame(
    "idu" = c("01001000010001", "02002000020002","03003000030003"),
    "surf_tot" = 1:3,
    "prop" = c("A", "B", "C"),
    "lieu_dit" =  c("A", "B", "C")
  )
}

test_that("normalize_legal_entity returns a data.frame", {

  res <- normalize_legal_entity(fake_data(), verbose = FALSE)

  expect_s3_class(res, "data.frame")
})

test_that("normalize_legal_entity preserves number of rows", {

  res <- normalize_legal_entity(fake_data(), verbose = FALSE)

  expect_equal(nrow(res), nrow(fake_data()))
})

test_that("normalize_legal_entity explodes IDU into components", {

  res <- normalize_legal_entity(fake_data(), verbose = FALSE)

  fields <- c("insee", "com_code", "prefix", "section", "number")
  names <- lapply(fields, \(x) seq_field(x)$name) |> unlist()

  expect_all_true(names %in% names(res))

  expect_equal(res[[seq_field("insee")$name]], substr(res[[seq_field("idu")$name]], 1, 5))
})

test_that("normalize_legal_entity joins are non-destructive", {

  le <- fake_data()
  res <- normalize_legal_entity(le, verbose = FALSE)
  expect_equal(nrow(res), nrow(le))
})

test_that("normalize_legal_entity sets source URL", {

  res <- normalize_legal_entity(fake_data(), verbose = FALSE)

  expect_true(seq_field("source")$name %in% names(res))
  expect_true(all(grepl("^https://data.economie.gouv.fr", res$source)))
})

test_that("normalize_legal_entity enriches with cog info", {

  le <- fake_data()
  res <- normalize_legal_entity(le, verbose = FALSE)

  reg_code <- seq_field("reg_code")$name
  dep_code <- seq_field("dep_code")$name
  insee <- seq_field("insee")$name
  com_code <- seq_field("com_code")$name
  prefix <- seq_field("prefix")$name
  section <- seq_field("section")$name
  numero <- seq_field("number")$name

  expect_all_true(grepl("^[0-9]{2}$", res[[reg_code]]))
  expect_all_true(grepl("^([0-9]{2}|2A|2B)$", res[[dep_code]]))
  expect_all_true(grepl("^[0-9]{5}$", res[[insee]]))
  expect_all_true(grepl("^[0-9]{3}$", res[[com_code]]))
  expect_all_true(grepl("^[0-9]{1,3}$", res[[prefix]]))
  expect_all_true(grepl("^[0-9A-Z]{2}$", res[[section]]))
  expect_all_true(grepl("^[0-9]{1,4}$", res[[numero]]))

})

test_that("normalize_legal_entity convert contenance to hectare", {

  le <- fake_data()
  res <- normalize_legal_entity(le, verbose = FALSE)

  cad_area <- seq_field("cad_area")$name
  le_cad_area <- "surf_tot"
  expect_equal(sum(le[[le_cad_area]])/10000, sum(res[[cad_area]]))

})


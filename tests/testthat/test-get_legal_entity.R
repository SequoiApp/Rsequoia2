fake_data <- function() {
  # fake data should have 24 col but not all are read
  # col_classes <- replace(rep("NULL", 24), c(1, 3, 5, 6, 7, 13, 14, 16, 17, 18, 24), NA)
  data.frame(
    "dep" = c("1", "29", "29"),
    "2_null" = NA,
    "com" = c("1", "123", "123"),
    "4_null" = NA,
    "prefix" = c("", "123", "123"),
    "section" = c("A", "AB", "AB"),
    "numero" = c("0001", "1", "1"),
    "8_null" = NA,
    "9_null" = NA,
    "10_null" = NA,
    "11_null" = NA,
    "12_null" = NA,
    "lieu_dit" = c("LIEU1", "LIEU2", "LIEU3"),
    "surf_tot" = c("1", "1", "1"),
    "15_null" = NA,
    "nature" = c("L - Landes", "T - Terre", "T - Terre"),
    "contenance" = c("1", "1", "1"),
    "type" = c("P - Propriétaire", "P - Propriétaire", "P - Propriétaire"),
    "19_null" = NA,
    "20_null" = NA,
    "21_null" = NA,
    "22_null" = NA,
    "23_null" = NA,
    "prop" = c("PROP1", "PROP2", "PROP3")
  )
}

test_that("get_legal_entity() rejects invalid INSEE and department codes", {

  expect_error(get_legal_entity("99999"), "Invalid INSEE")
  expect_error(get_legal_entity("99"), "Invalid department")

})

test_that("get_legal_entity() warns on department-level queries", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(fake_data(), row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  expect_message(
    res <- get_legal_entity(x, cache = le_cache, verbose = TRUE),
    "Department-level queries may be slower"
  ) |> suppressMessages()

  expect_s3_class(res, "data.frame")
})

test_that("get_legal_entity() reads CSV files from cache", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(fake_data(), row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  res <- get_legal_entity("01", cache = le_cache, verbose = FALSE)

  expect_shape(res, dim = c(2, 16))
})

test_that("get_legal_entity() reads CSV files from cache", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(fake_data(), row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  res <- get_legal_entity("01001", cache = le_cache, verbose = FALSE)

  expect_shape(res, dim = c(1, 16))
})

test_that("get_legal_entity() keeps only owner entries", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  df <- fake_data()
  df$type[1] <- "L - Locataire"

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(df, row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  res <- get_legal_entity("01", cache = le_cache, verbose = FALSE)

  expect_shape(res, dim = c(1, 16))
})

test_that("get_legal_entity() aggregates prop and lieu_dit per IDU", {

  le_cache <- file.path(tempdir(), "legal_entity_agg")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(fake_data(), row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  res <- get_legal_entity("01", cache = le_cache, verbose = FALSE)

  prop_field <- seq_field("owner")$name
  lieu_dit_field <- seq_field("locality")$name

  expect_true(any(grepl("PROP2 \\ PROP3", res[[prop_field]], fixed = TRUE)))
  expect_true(any(grepl("LIEU2 \\ LIEU3", res[[lieu_dit_field]], fixed = TRUE)))
})

test_that("get_legal_entity() return right cog info", {

  le_cache <- file.path(tempdir(), "legal_entity_agg")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  x <- "01"
  path <- file.path(le_cache, paste0(x, ".csv"))
  write.csv2(fake_data(), row.names = FALSE, path, fileEncoding = "UTF-8")

  testthat::local_mocked_bindings(
    download_legal_entity = function(cache, verbose) le_cache
  )

  res <- get_legal_entity("01", cache = le_cache, verbose = FALSE)

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

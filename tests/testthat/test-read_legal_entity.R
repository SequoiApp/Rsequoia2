fake_data <- function(dep) {
  # fake data should have 24 col but not all are read
  # col_classes <- replace(rep("NULL", 24), c(1, 3, 5, 6, 7, 13, 14, 16, 17, 18, 24), NA)
  data.frame(
    "dep" = dep,
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

test_that("read_legal_entity() returns a shaped data.frame ", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  dep <- "1"
  path1 <- file.path(le_cache, paste0(dep, ".csv"))
  write.csv2(fake_data(dep), row.names = FALSE, path1, fileEncoding = "UTF-8")

  res <- read_legal_entity(path1, verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_shape(res, dim = c(3,11))

})

test_that("read_legal_entity() binds rows from multiple CSV files", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  dep <- "1"
  path1 <- file.path(le_cache, paste0(dep, ".csv"))
  write.csv2(fake_data(dep), row.names = FALSE, path1, fileEncoding = "UTF-8")

  dep <- "2"
  path2 <- file.path(le_cache, paste0(dep, ".csv"))
  write.csv2(fake_data(dep), row.names = FALSE, path2, fileEncoding = "UTF-8")

  files <- c(path1, path2)
  res <- read_legal_entity(files, verbose = FALSE)

  expect_shape(res, nrow = 6)
  expect_all_true(res$dep %in% 1:2)
})

test_that("read_legal_entity() sets canonical column names", {

  le_cache <- file.path(tempdir(), "legal_entity")
  dir.create(le_cache)
  on.exit(unlink(le_cache, recursive = TRUE, force = TRUE), add = TRUE)

  dep <- "1"
  path1 <- file.path(le_cache, paste0(dep, ".csv"))
  write.csv2(fake_data(dep), row.names = FALSE, path1, fileEncoding = "UTF-8")

  res <- read_legal_entity(path1, verbose = FALSE)

  expect_identical(
    names(res),
    c(
      "dep", "com", "prefix", "section", "numero", "lieu_dit",
      "surf_tot", "nature", "contenance", "type", "prop"
    )
  )
})

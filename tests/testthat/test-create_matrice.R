test_that("create_matrice() creates a valid Excel file", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  id <- "TEST"
  outfile <- file.path(seq_cache, paste0(id, "_matrice.xlsx"))

  create_matrice(seq_cache, id = id, verbose = FALSE)

  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)

  # Check that IDENTIFIANT matches id
  identifiant <- seq_field("identifiant")$name
  df <- openxlsx2::read_xlsx(outfile, sheet = "MATRICE")
  expect_equal(df[[identifiant]], id)
})

test_that("create_matrice() refuses to overwrite by default", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  id <- "TEST"
  create_matrice(seq_cache, id = id, verbose = FALSE)

  expect_error(
    create_matrice(seq_cache, id = id, verbose = FALSE),
    "exists"
  )
})

test_that("create_matrice() overwrites when requested", {

  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE))

  id <- "TEST"
  create_matrice(seq_cache, id = id, verbose = FALSE)

  expect_silent(create_matrice(seq_cache, id = id, overwrite = TRUE, verbose = FALSE))
})


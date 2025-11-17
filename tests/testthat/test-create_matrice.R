test_that("create_matrice() creates a valid Excel file", {
  tmpdir <- tempdir()
  id <- "TEST_FOREST"
  outfile <- file.path(tmpdir, paste0(id, "_matrice.xlsx"))

  on.exit(unlink(outfile), add = TRUE)

  create_matrice(tmpdir, id = id, verbose = FALSE)

  expect_true(file.exists(outfile))
  expect_gt(file.size(outfile), 0)

  # Check that IDENTIFIANT matches id
  df <- openxlsx2::read_xlsx(outfile, sheet = "MATRICE")
  expect_equal(df$IDENTIFIANT, id)
})

test_that("create_matrice() refuses to overwrite by default", {
  tmpdir <- tempdir()
  id <- "FOREST_X"
  outfile <- file.path(tmpdir, paste0(id, "_matrice.xlsx"))

  on.exit(unlink(outfile), add = TRUE)

  create_matrice(tmpdir, id = id, verbose = FALSE)

  expect_error(
    create_matrice(tmpdir, id = id, verbose = FALSE),
    "exists"
  )
})

test_that("create_matrice() overwrites when requested", {
  tmpdir <- tempdir()
  id <- "FOREST_Y"
  outfile <- file.path(tmpdir, paste0(id, "_matrice.xlsx"))

  on.exit(unlink(outfile), add = TRUE)

  create_matrice(tmpdir, id = id, verbose = FALSE)

  expect_silent(create_matrice(tmpdir, id = id, overwrite = TRUE, verbose = FALSE))
})

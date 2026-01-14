test_that("create_matrice() creates a valid Excel file", {
  with_seq_cache({
    id <- "TEST"
    outfile <- create_matrice(seq_cache, id = id, verbose = FALSE)

    expect_true(file.exists(outfile))
    expect_gt(file.size(outfile), 0)

    # Check that IDENTIFIANT matches id
    identifier <- seq_field("identifier")$name
    df <- openxlsx2::read_xlsx(outfile, sheet = "MATRICE")
    expect_equal(df[[identifier]], id)
  })
})

test_that("create_matrice() refuses to overwrite by default", {
  with_seq_cache({
    id <- "TEST"
    outfile <- create_matrice(seq_cache, id = id, verbose = FALSE)

  expect_error(
    create_matrice(seq_cache, id = id, verbose = FALSE),
    "exists"
    )
  })
})

test_that("create_matrice() overwrites when requested", {
  with_seq_cache({
    id <- "TEST"
    outfile <- create_matrice(seq_cache, id = id, verbose = FALSE)

    expect_silent(create_matrice(seq_cache, id = id, overwrite = TRUE, verbose = FALSE))
  })
})


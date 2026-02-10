test_that("get_drias_table() validates txt argument", {
  # Non-character
  expect_error(get_drias_table(42), class = "drias_invalid_txt")
  # NA
  expect_error(get_drias_table(NA_character_), class = "drias_invalid_txt")
  # Empty string
  expect_error(get_drias_table(""), class = "drias_invalid_txt")
  # Non-existent file
  expect_error(get_drias_table("does_not_exist.txt"), class = "drias_file_not_found")
})

test_that("get_drias_table() errors on invalid metadata", {
  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  # Fichier minimal pour passer file.exists
  writeLines("X;Y;", txt)

  # indices non-data.frame
  local_mocked_bindings(
    get_drias_metadata = function(txt) list(indices = "not a data.frame")
  )
  expect_error(get_drias_table(txt), class = "drias_invalid_metadata")

  # indices sans colonne code
  local_mocked_bindings(
    get_drias_metadata = function(txt) list(indices = data.frame(foo = 1))
  )
  expect_error(get_drias_table(txt), class = "drias_invalid_metadata")

  # indices avec NA
  local_mocked_bindings(
    get_drias_metadata = function(txt) list(indices = data.frame(code = c("A", NA)))
  )
  expect_error(get_drias_table(txt), class = "drias_invalid_metadata")

  # indices avec doublons
  local_mocked_bindings(
    get_drias_metadata = function(txt) list(indices = data.frame(code = c("A", "A")))
  )
  expect_error(get_drias_table(txt), class = "drias_invalid_metadata")
})

test_that("get_drias_table() errors if column count mismatch", {
  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  # fichier avec 3 colonnes
  writeLines(c("1;2;3"), txt)

  local_mocked_bindings(
    get_drias_metadata = function(txt) list(indices = data.frame(code = c("A", "B")))
  )

  expect_error(get_drias_table(txt), class = "drias_table_mismatch")
})

test_that("get_drias_table() returns correct table structure", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  # Example file with 3 indices
  writeLines("1;48.5;2.3;forest;H0;1;10;20;30", tmp)

  local_mocked_bindings(
    get_drias_metadata = function(txt) list(
      indices = data.frame(code = c("IDX1", "IDX2", "IDX3"))
    )
  )

  tbl <- get_drias_table(tmp)

  # Check column names including fixed columns
  expect_equal(
    names(tbl),
    c("Point", "Latitude", "Longitude", "Contexte", "Periode", "Mois",
      "IDX1", "IDX2", "IDX3")
  )

  # Check class
  expect_s3_class(tbl, "data.frame")
})

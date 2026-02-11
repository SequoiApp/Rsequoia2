test_that("txt validation fails early and cleanly", {

  expect_error(drias_read_table(42), "must be a character string")
  expect_error(drias_read_table(NA_character_), "must be a non-empty character")
  expect_error(drias_read_table(""), "must be a non-empty character string")
  expect_error(drias_read_table("does_not_exist.txt"),"File not found")

})

test_that("drias_read_table parses valid DRIAS file correctly", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Some header info",
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;NORRR;",
      "123;47.5;-1.5;1;H1;10.5;50",
      "123;47.5;-1.5;2;H1;11.0;60"
    ),
    txt,
    useBytes = TRUE
  )

  df <- drias_read_table(txt)

  expect_s3_class(df, "data.frame")

  expect_named(
    df,
    c("POINT", "LATITUDE", "LONGITUDE", "MOIS", "PERIODE", "NORTAV", "NORRR")
  )

  expect_equal(nrow(df), 2L)

  expect_equal(df$POINT, c(123, 123))
  expect_equal(df$MOIS, c(1, 2))
  expect_equal(df$NORTAV, c(10.5, 11.0))
})

test_that("empty columns are removed", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Point;Latitude;Longitude;NORTAV;EMPTY;",
      "123;47.5;-1.5;10.5;",
      "123;47.5;-1.5;11.0;"
    ),
    txt,
    useBytes = TRUE
  )

  df <- drias_read_table(txt)

  expect_false("EMPTY" %in% names(df))
})

test_that("fails cleanly if header not found", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines("123;47.5;10.5", txt)

  expect_error(drias_read_table(txt), "No colname line starting")
})

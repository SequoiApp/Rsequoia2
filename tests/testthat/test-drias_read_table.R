test_that("txt validation fails early and cleanly", {

  expect_error(drias_read_table(42), "must be a character string")
  expect_error(drias_read_table(NA_character_), "must be a non-empty character")
  expect_error(drias_read_table(""), "must be a non-empty character string")
  expect_error(drias_read_table("does_not_exist.txt"), "File not found")

})

test_that("drias_read_table parses valid DRIAS file correctly", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Some header info",
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;NORRR;ATAV;ARR",
      "123;47.5;-1.5;1;H1;10.5;50;1.0;5",
      "123;47.5;-1.5;2;H1;11.0;60;2.0;6"
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

  ## original rows + reconstructed H0
  expect_equal(nrow(df), 4L)

  expect_true("H0" %in% df$PERIODE)
  expect_true("H1" %in% df$PERIODE)

})

test_that("H0 is correctly reconstructed from H1", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;ATAV",
      "123;47.5;-1.5;1;H1;10;2",
      "123;47.5;-1.5;2;H1;20;5"
    ),
    txt,
    useBytes = TRUE
  )

  df <- drias_read_table(txt)

  h0 <- df[df$PERIODE == "H0", ]

  expect_equal(h0$NORTAV, c(8, 15))

})

test_that("empty columns are removed", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;ATAV;EMPTY",
      "123;47.5;-1.5;1;H1;10;1;",
      "123;47.5;-1.5;2;H1;20;2;"
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

test_that("works with multiple NOR variables", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;ATAV;NORRR;ARR",
      "123;47.5;-1.5;1;H1;10;1;100;10"
    ),
    txt,
    useBytes = TRUE
  )

  df <- drias_read_table(txt)

  h0 <- df[df$PERIODE == "H0", ]

  expect_equal(h0$NORTAV, 9)
  expect_equal(h0$NORRR, 90)

})

test_that("returns only NOR variables and core metadata", {

  txt <- tempfile(fileext = ".txt")
  on.exit(unlink(txt))

  writeLines(
    c(
      "# Point;Latitude;Longitude;MOIS;PERIODE;NORTAV;ATAV;FOO",
      "123;47.5;-1.5;1;H1;10;1;999"
    ),
    txt,
    useBytes = TRUE
  )

  df <- drias_read_table(txt)

  expect_false("FOO" %in% names(df))
  expect_false("ATAV" %in% names(df))

})

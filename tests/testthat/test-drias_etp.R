test_that("drias_etp computes precipitation, ETP and water balance correctly", {

  txt <- tempfile(fileext = ".txt")

  fake_meta <- list(
    horizon = data.frame(
      code = "H1",
      start = 2021,
      end = 2050,
      stringsAsFactors = FALSE
    )
  )

  fake_drias <- data.frame(
    POINT = rep(c("P1", "P2"), each = 12),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORETPC = rep(20, 24),
    NORRR = rep(50, 24),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_etp(txt)

  expect_s3_class(res, "data.frame")

  expect_named(
    res,
    c("PERIODE", "MOIS", "NORRR", "NORETPC", "P-ETP")
  )

  expect_equal(res$NORRR, rep(50, 12))
  expect_equal(res$NORETPC, rep(20, 12))
  expect_equal(res$`P-ETP`, rep(30, 12))
})

test_that("drias_etp aggregates correctly across POINT", {

  txt <- tempfile()

  fake_meta <- list(
    horizon = data.frame(code = "H1", start = 2021, end = 2050)
  )

  fake_drias <- data.frame(
    POINT = rep(c("P1", "P2"), each = 12),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORETPC = c(rep(10, 12), rep(30, 12)),
    NORRR = c(rep(40, 12), rep(80, 12))
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_etp(txt)

  expect_equal(res$NORRR, rep(60, 12))
  expect_equal(res$NORETPC, rep(20, 12))
  expect_equal(res$`P-ETP`, rep(40, 12))
})

test_that("POINT with zero-only values is removed", {

  txt <- tempfile()

  fake_meta <- list(
    horizon = data.frame(code = "H1", start = 2021, end = 2050)
  )

  fake_drias <- data.frame(
    POINT = c(rep("VALID", 12), rep("INVALID", 12)),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORETPC = c(rep(20, 12), rep(0, 12)),
    NORRR = c(rep(50, 12), rep(0, 12))
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_etp(txt)

  expect_equal(res$NORRR, rep(50, 12))
})

test_that("P-ETP calculation is correct", {

  txt <- tempfile()

  fake_meta <- list(
    horizon = data.frame(code = "H1", start = 2021, end = 2050)
  )

  fake_drias <- data.frame(
    POINT = rep("P1", 12),
    MOIS = 1:12,
    PERIODE = "H1",
    NORETPC = 1:12,
    NORRR = 13:24
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_etp(txt)

  expect_equal(res$`P-ETP`, rep(12, 12))
})

test_that("output structure is stable", {

  txt <- tempfile()

  fake_meta <- list(
    horizon = data.frame(code = "H1", start = 2021, end = 2050)
  )

  fake_drias <- data.frame(
    POINT = rep("P1", 12),
    MOIS = 1:12,
    PERIODE = "H1",
    NORETPC = 1:12,
    NORRR = 13:24
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_etp(txt)

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 12)

  expect_named(
    res,
    c("PERIODE", "MOIS", "NORRR", "NORETPC", "P-ETP")
  )
})



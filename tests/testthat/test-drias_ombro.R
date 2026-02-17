test_that("drias_ombro computes monthly means correctly", {

  txt <- tempfile(fileext = ".txt")

  fake_meta <- list(
    horizon = data.frame(
      code = c("H1"),
      start = 2021,
      end = 2050,
      stringsAsFactors = FALSE
    )
  )

  fake_drias <- data.frame(
    POINT = rep(c("P1", "P2"), each = 12),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORTAV = rep(10, 24),
    NORTNAV = rep(5, 24),
    NORTXAV = rep(15, 24),
    NORRR = rep(50, 24),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_ombro(txt)

  expect_s3_class(res, "data.frame")

  expect_named(
    res, c("LABEL", "MOIS", "NORTAV", "NORTNAV", "NORTXAV", "NORRR")
  )

  expect_equal(unique(res$LABEL), "2021-2050")

  expect_equal(res$NORTAV, rep(10, 12))
  expect_equal(res$NORTNAV, rep(5, 12))
  expect_equal(res$NORTXAV, rep(15, 12))
  expect_equal(res$NORRR, rep(50, 12))
})

test_that("POINT with zero-only variables is removed", {

  txt <- tempfile(fileext = ".txt")

  fake_meta <- list(
    horizon = data.frame(
      code = "H1",
      start = 2021,
      end = 2050
    )
  )

  fake_drias <- data.frame(
    POINT = c(rep("VALID", 12), rep("INVALID", 12)),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORTAV = c(rep(10, 12), rep(0, 12)),
    NORTNAV = c(rep(5, 12), rep(0, 12)),
    NORTXAV = c(rep(15, 12), rep(0, 12)),
    NORRR = c(rep(50, 12), rep(0, 12))
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_ombro(txt)

  expect_equal(res$NORTAV, rep(10, 12))
})

test_that("aggregation computes correct mean across POINT", {

  txt <- tempfile(fileext = ".txt")

  fake_meta <- list(
    horizon = data.frame(
      code = "H1",
      start = 2021,
      end = 2050
    )
  )

  fake_drias <- data.frame(
    POINT = rep(c("P1", "P2"), each = 12),
    MOIS = rep(1:12, 2),
    PERIODE = "H1",
    NORTAV = c(rep(10, 12), rep(20, 12)),
    NORTNAV = c(rep(5, 12), rep(15, 12)),
    NORTXAV = c(rep(15, 12), rep(25, 12)),
    NORRR = c(rep(50, 12), rep(100, 12))
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_ombro(txt)

  expect_equal(res$NORTAV, rep(15, 12))
  expect_equal(res$NORRR, rep(75, 12))
})

test_that("output structure is correct", {

  txt <- tempfile()

  fake_meta <- list(
    horizon = data.frame(code = "H1", start = 2021, end = 2050)
  )

  fake_drias <- data.frame(
    POINT = rep("P1", 12),
    MOIS = 1:12,
    PERIODE = "H1",
    NORTAV = 1:12,
    NORTNAV = 1:12,
    NORTXAV = 1:12,
    NORRR = 1:12
  )

  local_mocked_bindings(
    drias_read_metadata = function(...) fake_meta,
    drias_read_table = function(...) fake_drias
  )

  res <- drias_ombro(txt)

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 12)
  expect_true(all(c("LABEL", "MOIS") %in% names(res)))
})


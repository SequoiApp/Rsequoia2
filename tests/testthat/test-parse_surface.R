test_that("parse_surface() returns a named integer vector of length 3", {
  res <- parse_surface("121 58 04 510,46")

  expect_type(res, "integer")
  expect_length(res, 3)
  expect_named(res, c("ha", "a", "ca"))
})

test_that("parse_surface() works without decimal number", {
  expect_equal(parse_surface("121 58 04"), c(ha = 121L, a = 58L, ca = 4L))
  expect_equal(parse_surface("1 58 04"), c(ha = 1L, a = 58L, ca = 4L))
  expect_equal(parse_surface("58 04"),     c(ha = 0L,   a = 58L, ca = 4L))
  expect_equal(parse_surface("04"),        c(ha = 0L,   a = 0L,  ca = 4L))
})

test_that("parse_surface() handles empty or non-numeric input without decimal", {
  expect_equal(parse_surface("foo bar"), c(ha = 0L, a = 0L, ca = 0))
})

test_that("parse_surface() works with simple decimal revenue", {
  expect_equal(
    parse_surface("121 58 04 510,46"),
    c(ha = 121L, a = 58L, ca = 4L)
  )

  expect_equal(
    parse_surface("58 04 510,46"),
    c(ha = 0L, a = 58L, ca = 4L)
  )

  expect_equal(
    parse_surface("04 510,46"),
    c(ha = 0L, a = 0L, ca = 4L)
  )
})

test_that("parse_surface() correctly removes thousands prefix (1-digit)", {
  expect_equal(
    parse_surface("121 58 04 1 510,46"),
    c(ha = 121L, a = 58L, ca = 4L)
  )

  expect_equal(
    parse_surface("58 04 1 510,46"),
    c(ha = 0L, a = 58L, ca = 4L)
  )

  expect_equal(
    parse_surface("1 510,46"),
    c(ha = 0L, a = 0L, ca = 0L)
  )
})

test_that("parse_surface() works on real life exemple", {
  l0 <- "22 21 15 214,66 C TA 64,40 30"
  expect_equal(parse_surface(l0), c("ha" = 22, "a" = 21, "ca" = 15))

  l1 <- "8 52 00"
  expect_equal(parse_surface(l1), c("ha" = 8, "a" = 52, "ca" = 00))

  l2 <- "35 13 65"
  expect_equal(parse_surface(l2), c("ha" = 35, "a" = 13, "ca" = 65))

  l3 <- "09 42 0,13 C TA 0,04 30"
  expect_equal(parse_surface(l3), c("ha" = 0, "a" = 09, "ca" = 42))

  l4 <- "05 76 0,08 C TA 0,02 30"
  expect_equal(parse_surface(l4), c("ha" = 0, "a" = 05, "ca" = 76))

  l5 <- "09 01 0,00"
  expect_equal(parse_surface(l5), c("ha" = 0, "a" = 09, "ca" = 01))

  l6 <- "06 0,08 C TA 0,02 30"
  expect_equal(parse_surface(l0), c("ha" = 22, "a" = 21, "ca" = 15))

  l7 <- "11 42 0,00"
  expect_equal(parse_surface(l7), c("ha" = 0, "a" = 11, "ca" = 42))

  l8 <- "12 61 0,19 C TA 0,06 30"
  expect_equal(parse_surface(l8), c("ha" = 0, "a" = 12, "ca" = 61))

  l9 <- "02 90 0,00"
  expect_equal(parse_surface(l9), c("ha" = 0, "a" = 02, "ca" = 90))

  l10 <- "121 58 04 1 510,46 C TA 302,09 20"
  expect_equal(parse_surface(l10), c("ha" = 121, "a" = 58, "ca" = 04))

  l11 <- "6 21 52 259,88 C TA 51,98 20"
  expect_equal(parse_surface(l11), c("ha" = 6, "a" = 21, "ca" = 52))

  l12 <-"12 13 03 67,76 C TA 13,55 20"
  expect_equal(parse_surface(l12), c("ha" = 12, "a" = 13, "ca" = 03))

  l13 <- "43 40 0,42 C TA 0,08 20"
  expect_equal(parse_surface(l13), c("ha" = 0, "a" = 43, "ca" = 40))

})

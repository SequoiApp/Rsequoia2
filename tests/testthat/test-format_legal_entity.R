fake_data <- function(dep) {
  data.frame(
    "dep" = dep,
    "com" = c("1", "123", "123"),
    "prefix" = c("", "123", "123"),
    "section" = c("A", "AB", "AB"),
    "numero" = c("0001", "1", "1"),
    "lieu_dit" = c("LIEU1", "LIEU2", "LIEU3"),
    "surf_tot" = c("1", "1", "1"),
    "nature" = c("L - Landes", "T - Terre", "T - Terre"),
    "contenance" = c("1", "1", "1"),
    "type" = "P - Propriétaire",
    "prop" = "PROP"
  )
}

test_that("format_legal_entity() returns one row per IDU", {

  le <- fake_data(dep = "1")

  res <- format_legal_entity(le, verbose = FALSE)

  expect_equal(nrow(res), length(unique(res$idu)))
})

test_that("format_legal_entity builds valid IDU", {

  le <- fake_data(dep = "1")

  res <- format_legal_entity(le, verbose = FALSE)

  expect_true(all(nchar(res$idu) == 14))
  expect_match(res$idu[1], "^[0-9A-Z]{14}$")
})

test_that("format_legal_entity() keeps proprietors only", {

  le <- fake_data("1")
  le$type[1] <- "bad_type"
  le$prop[1] <- "bad_prop"

  res <- format_legal_entity(le, verbose = FALSE)

  expect_false(grepl("bad_prop", res$prop))
})

test_that("format_legal_entity() aggregates proprietors per IDU", {

  le <- fake_data("1")
  le$prop <- letters[1:3]

  res <- format_legal_entity(le, verbose = FALSE)

  expect_true("prop" %in% names(res))
  expect_match(res$prop[1], "a")
  expect_false(any(duplicated(res$idu)))
  expect_true(any(grepl("b \\ c", res$prop, fixed = TRUE)))
})

test_that("format_legal_entity() aggregates lieu_dit per IDU", {

  le <- fake_data("1")
  le$lieu_dit <- letters[1:3]

  res <- format_legal_entity(le, verbose = FALSE)

  expect_true("lieu_dit" %in% names(res))
  expect_match(res$lieu_dit[1], "a")
  expect_false(any(duplicated(res$idu)))
  expect_true(any(grepl("b \\ c", res$lieu_dit, fixed = TRUE)))
})

test_that("format_legal_entity filters by INSEE code", {

  le1 <- fake_data("1")
  le2 <- fake_data("29")

  le <- rbind(le1, le2)

  res <- format_legal_entity(le, code_insee = "01001", verbose = FALSE)

  expect_true(all(substr(res$idu, 1, 5) == "01001"))
})

test_that("format_legal_entity preserves surf_tot values", {

  le <- fake_data("1")

  res <- format_legal_entity(le, verbose = FALSE)

  expect_true("surf_tot" %in% names(res))
  expect_equal(res$surf_tot, c("1", "1"))
})

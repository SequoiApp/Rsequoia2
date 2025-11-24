test_that("field_rename() replaces aliases correctly", {

  fake_field <- list(
    field1 = list(name = "FIELD1", alias = c("ALIAS_FIELD1")),
    field2 = list(name = "FIELD2", alias = c("ALIAS_FIELD2", "ALIAS2_FIELD2"))
  )

  local_mocked_bindings(
    seq_field = function() fake_field
  )

  # Data using aliases
  df <- data.frame(
    ALIAS_FIELD1 = NA_character_,
    ALIAS_FIELD2 = NA_character_,
    other = NA_character_
  )

  res <- field_rename(df)

  expect_named(res, c("FIELD1", "FIELD2", "other"))
})

test_that("field_rename() abort when duplicated rename", {

  fake_field <- list(
    field1 = list(name = "FIELD1", alias = c("ALIAS_FIELD1")),
    field2 = list(name = "FIELD2", alias = c("ALIAS_FIELD2", "ALIAS2_FIELD2"))
  )

  local_mocked_bindings(
    seq_field = function() fake_field
  )

  # Data using aliases
  df <- data.frame(
    ALIAS_FIELD1 = NA_character_,
    ALIAS_FIELD2 = NA_character_,
    ALIAS2_FIELD2 = NA_character_,
    other = NA_character_
  )

  expect_error(
    field_rename(df),
    "Multiple columns would be renamed.*ALIAS_FIELD2 -> FIELD2"
  )

})

test_that("field_check_class() coerces fields properly", {

  fake_field <- list(
    field1 = list(name = "FIELD_NUM1", class = "numeric"),
    field2 = list(name = "FIELD_NUM2", class = "numeric"),
    field3 = list(name = "FIELD_CHAR1", class = "character")
  )

  local_mocked_bindings(
    seq_field = function() fake_field
  )

  df <- data.frame(
    FIELD_NUM1 = "1,2", # "," replace with "." if numeric
    FIELD_NUM2 = "1.2",
    FIELD_CHAR1 = 1.2
  )
  res <- field_check_class(df)

  expect_equal(res$FIELD_NUM1, 1.2)
  expect_equal(res$FIELD_NUM2, 1.2)
  expect_equal(res$FIELD_CHAR1, "1.2")
})

test_that("field_order() works", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2")
  )

  local_mocked_bindings(
    seq_field = function() fake_field,
    seq_table = function(table) c("field1", "field2"),
  )

  # Data using aliases
  df <- data.frame(
    FIELD2 = NA_character_,
    FIELD1 = NA_character_,
    other = NA_character_
  )

  res <- field_order(df, "table1")

  expect_equal(names(res), c("FIELD1", "FIELD2", "other"))

})

test_that("field_add_drop() works", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2")
  )

  local_mocked_bindings(
    seq_field = function() fake_field,
    seq_table = function(table) "field1",
  )

  # Data using aliases
  df <- data.frame(
    FIELD2 = NA_character_,
    FIELD1 = NA_character_,
    other = NA_character_
  )

  res <- field_add_drop(df, "table")

  expect_equal(names(res), c("FIELD1"))

})

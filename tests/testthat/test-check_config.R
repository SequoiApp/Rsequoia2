test_that("check_config() passes when everything is valid", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2")
  )

  fake_table <- list(
    table1 = c("field1", "field2"),
    table2 = c("field2")
  )

  local_mocked_bindings(
    seq_field = function(...) fake_field,
    seq_table = function() fake_table
  )

  expect_no_warning(res <- check_config() |> suppressMessages())

  expect_named(res, c("table_bad_keys", "unused_fields"))
  expect_true(all(lengths(res$table_bad_keys) == 0))
  expect_length(res$unused_fields, 0)
})

test_that("check_config() warns when a table contains invalid fields", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2")
  )

  fake_table <- list(
    table1 = c("field1", "bad_field"),
    table2 = c("field2")
  )

  local_mocked_bindings(
    seq_field = function(...) fake_field,
    seq_table = function() fake_table
  )

  expect_warning(res <- check_config(), "bad_field")

  expect_equal(res$table_bad_keys$table1, "bad_field")
  expect_length(res$unused_fields, 0)
})

test_that("check_config() warns when fields are unused", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2"),
    unused = list(name = "UNUSED")
  )

  fake_table <- list(
    table1 = c("field1"),
    table2 = c("field2")
  )

  local_mocked_bindings(
    seq_field = function(...) fake_field,
    seq_table = function() fake_table
  )

  expect_warning(res <- check_config(), "unused")

  expect_equal(res$unused_fields, "unused")
})

test_that("check_config() finds both invalid and unused fields", {

  fake_field <- list(
    field1 = list(name = "FIELD1"),
    field2 = list(name = "FIELD2"),
    unused = list(name = "ORPHAN")
  )

  fake_table <- list(
    table1 = c("field1", "bad_key1"),
    table2 = c("field2", "bad_key2")
  )

  local_mocked_bindings(
    seq_field = function(...) fake_field,
    seq_table = function() fake_table
  )

  expect_warning(res <- check_config(), "bad_key1") |> suppressWarnings()
  expect_warning(res <- check_config(), "bad_key2") |> suppressWarnings()
  expect_warning(res <- check_config(), "unused") |> suppressWarnings()

  expect_equal(sort(res$table_bad_keys$table1), "bad_key1")
  expect_equal(sort(res$table_bad_keys$table2), "bad_key2")
  expect_equal(res$unused_fields, "unused")
})

test_that("seq_table() returns full config when table = NULL", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_tables.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    table1 = c("field1", "field2"),
    table2 = c("field1", "field2")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  res <- seq_table(filepath = cfg_path)

  expect_type(res, "list")
  expect_named(res, c("table1", "table2"))
  expect_equal(res, fake_cfg)
})

test_that("seq_table() returns a single table entry", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_tables.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    table1 = c("field1", "field2"),
    table2 = c("field1", "field2")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  res <- seq_table(table = "table1", filepath = cfg_path)

  expect_equal(res, c("field1", "field2"))
})

test_that("seq_table() errors on invalid table name", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_tables.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    table1 = c("field1", "field2"),
    table2 = c("field1", "field2")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  expect_error(
    seq_table(table = "bad_table_name", filepath = cfg_path),
    "Available tables are"
  )

  expect_error(
    seq_table(table = c("bad_table_name", "bad_table_name2"), filepath = cfg_path),
    "should be length one character from"
  )
})

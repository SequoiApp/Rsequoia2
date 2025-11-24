test_that("seq_field() returns full config when field = NULL", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_fields.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    id = list(name = "IDENTIFIANT", alias = c("id", "foret")),
    proprio = list(name = "PROPRIETAIRE", alias = "owner")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  res <- seq_field(filepath = cfg_path)

  expect_type(res, "list")
  expect_named(res, c("id", "proprio"))
  expect_equal(res, fake_cfg)
})

test_that("seq_field() returns a single field entry", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_fields.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    id = list(name = "IDENTIFIANT", alias = c("id", "foret")),
    proprio = list(name = "PROPRIETAIRE", alias = "owner")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  res <- seq_field(field = "id", filepath = cfg_path)

  expect_type(res, "list")
  expect_equal(res$name, "IDENTIFIANT")
  expect_equal(res$alias, c("id", "foret"))
})

test_that("seq_field() errors on invalid field name", {

  # Create a temporary config file
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_fields.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    id = list(name = "IDENTIFIANT", alias = c("id", "foret")),
    proprio = list(name = "PROPRIETAIRE", alias = "owner")
  )

  yaml::write_yaml(fake_cfg, cfg_path)

  expect_error(
    seq_field(field = "xxx", filepath = cfg_path),
    "Bad `field` value"
  )

})

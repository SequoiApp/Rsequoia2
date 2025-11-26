test_that("get_keys() returns all keys when pattern = NULL", {
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_layers.yaml")
  on.exit(unlink(cfg_path))

  fake_cfg <- list(
    "v.seq.parca.poly" = list(),
    "r.ortho.irc"      = list()
  )
  yaml::write_yaml(fake_cfg, cfg_path)

  res <- get_keys(pattern = NULL, filepath = cfg_path)

  expect_type(res, "character")
  expect_setequal(res, names(fake_cfg))
})


test_that("get_keys() filters keys by pattern", {
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_layers.yaml")
  on.exit(unlink(cfg_path, recursive = TRUE))

  fake_cfg <- list(
    "v.seq.parca.poly" = list(),
    "r.ortho.irc"      = list()
  )
  yaml::write_yaml(fake_cfg, cfg_path)

  res <- get_keys(pattern = "parca", reduce = FALSE, filepath = cfg_path)

  expect_equal(res, "v.seq.parca.poly")
})


test_that("get_keys() reduces keys correctly", {
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_layers.yaml")
  on.exit(unlink(cfg_path, recursive = TRUE))

  fake_cfg <- list(
    "v.seq.parca.poly" = list(),
    "r.ortho.irc"      = list()
  )
  yaml::write_yaml(fake_cfg, cfg_path)

  res <- get_keys(pattern = "parca", reduce = TRUE, filepath = cfg_path)

  expect_equal(res, "parca")   # 3rd element of v.seq.parca.poly
})


test_that("get_keys() aborts when reduced keys are duplicated", {
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_layers.yaml")
  on.exit(unlink(cfg_path, recursive = TRUE))

  fake_cfg <- list(
    "v.seq.parca.point" = list(),
    "v.seq.parca.poly" = list()
  )
  yaml::write_yaml(fake_cfg, cfg_path)

  expect_error(
    get_keys(pattern = "parca", reduce = TRUE, filepath = cfg_path),
    "duplicated"
  )
})


test_that("get_keys() aborts when pattern does not match anything", {
  tmp <- tempdir()
  cfg_path <- file.path(tmp, "seq_layers.yaml")
  on.exit(unlink(cfg_path, recursive = TRUE))

  fake_cfg <- list(
    "v.seq.parca.poly" = list(),
    "r.ortho.irc"      = list()
  )
  yaml::write_yaml(fake_cfg, cfg_path)

  expect_error(
    get_keys(pattern = "nope", filepath = cfg_path),
    "does not exist"
  )
})

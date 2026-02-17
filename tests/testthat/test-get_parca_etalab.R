test_that("get_parca_etalab() aborts on empty idu", {

  expect_error(get_parca_etalab(character()), "idu.*non-empty")
  expect_error(get_parca_etalab(NULL), "idu.*non-empty")
  expect_error(get_parca_etalab(numeric()), "idu.*non-empty")

  expect_error(get_parca_etalab(Rsequoia2:::seq_poly), "must be")
  expect_error(get_parca_etalab(list("aaa")), "must be")

})

test_that("get_parca_etalab() abort when all idu are invalid", {

  idu <- c("352890000A0145", "352890000A0147")
  local_mocked_bindings(
    read_sf = function(...) data.frame(
      id = "other_idu",
      prefixe = "000",
      section = "0A",
      numero = "0001"
    ),
    .package = "sf"
  )

  expect_error(get_parca_etalab(idu), "Invalid idu detected:")

})

test_that("get_parca_etalab() abort when any idu are invalid", {

  idu <- c("352890000A0145", "352890000A0147")
  local_mocked_bindings(
    read_sf = function(...) data.frame(
      id = "352890000A0145",
      prefixe = "000",
      section = "0A",
      numero = "0001"
    ),
    .package = "sf"
  )

  expect_error(get_parca_etalab(idu), "Invalid idu detected:.*352890000A0147")

})

test_that("get_parca_etalab() de-duplicates idu", {

  idu <- c("010010000A0001", "010010000A0001")

  calls <- 0
  local_mocked_bindings(
    read_sf = function(...) {
      calls <<- calls + 1
      Rsequoia2:::seq_poly |>
        transform(
          id = "010010000A0001",
          commune = "01001",
          prefixe = "000",
          section = "0A",
          numero = "0001",
          contenance = 10
        )
    },
    .package = "sf"
  )

  local_mocked_bindings(
    get_cog = function(...) list(
      com = data.frame(COM = "01001", NCC_COM = "NCC_01001", DEP = "01"),
      dep = data.frame(DEP = "01", NCC_DEP = "NCC_01", REG = "01"),
      reg = data.frame(REG = "01", NCC_REG = "NCC_01")
      )
  )

  res <- get_parca_etalab(idu)

  expect_equal(calls, 1)
})

test_that("get_parca_etalab() pads cadastral fields", {

  idu <- "010010000A0001"
  fake <- Rsequoia2:::seq_poly |>
    transform(
      id = idu,
      commune = "1001",   # intentionally unpadded
      prefixe = "1",
      section = "A",
      numero  = "1",
      contenance = 10
    )

  local_mocked_bindings(
    read_sf = function(...) fake,
    .package = "sf"
  )

  local_mocked_bindings(
    get_cog = function(...) list(
      com = data.frame(COM = "01001", NCC_COM = "NCC_01001", DEP = "01"),
      dep = data.frame(DEP = "01", NCC_DEP = "NCC_01", REG = "01"),
      reg = data.frame(REG = "01", NCC_REG = "NCC_01")
    )
  )

  res <- get_parca_etalab(idu)

  f <- \(x) seq_field(x)$name
  expect_all_equal(res[[f("prefix")]], "001")
  expect_all_equal(res[[f("section")]], "0A")
  expect_all_equal(res[[f("number")]], "0001")
  expect_all_equal(res[[f("insee")]], "01001")
})

test_that("get_parca_etalab() add source", {

  idu <- c("010010000A0001")
  fake_parca_etalab <- Rsequoia2:::seq_poly |>
    transform(
      "id" = idu,
      "commune" = "01001",
      "prefixe" = "000",
      "section" = "0A",
      "numero" = "0001",
      "contenance" = 10
    )

  local_mocked_bindings(
    read_sf = function(...) fake_parca_etalab,
    .package = "sf"
  )

  local_mocked_bindings(
    get_cog = function(...) list(
      com = data.frame(COM = "01001", NCC_COM = "NCC_01001", DEP = "01"),
      dep = data.frame(DEP = "01", NCC_DEP = "NCC_01", REG = "01"),
      reg = data.frame(REG = "01", NCC_REG = "NCC_01")
    )
  )

  res <- get_parca_etalab(idu)
  source <- seq_field("source")$name

  expect_s3_class(res, "sf")
  expect_all_equal(res[[source]], "etalab")
})

test_that("get_parca_etalab() convert contenance to ha", {

  idu <- c("010010000A0001")
  fake_parca_etalab <- Rsequoia2:::seq_poly |>
    transform(
      "id" = idu,
      "commune" = "01001",
      "prefixe" = "000",
      "section" = "0A",
      "numero" = "0001",
      "contenance" = 10
    )

  local_mocked_bindings(
    read_sf = function(...) fake_parca_etalab,
    .package = "sf"
  )

  local_mocked_bindings(
    get_cog = function(...) list(
      com = data.frame(COM = "01001", NCC_COM = "NCC_01001", DEP = "01"),
      dep = data.frame(DEP = "01", NCC_DEP = "NCC_01", REG = "01"),
      reg = data.frame(REG = "01", NCC_REG = "NCC_01")
    )
  )

  res <- get_parca_etalab(idu)
  cad_area <- seq_field("cad_area")$name

  expect_s3_class(res, "sf")
  expect_all_equal(res[[cad_area]], 0.001)
})

test_that("get_parca_etalab() queries one URL per unique commune", {

  idu <- c("010010000A0001", "010020000A0001", "010030000A0001")
  urls <- character()

  local_mocked_bindings(
    read_sf = function(x, ...) {
      urls <<- c(urls, x)
      Rsequoia2:::seq_poly |>
        transform(
          id = idu,
          commune = "01001",
          prefixe = "000",
          section = "0A",
          numero = "0001",
          contenance = 10
        )
    },
    .package = "sf"
  )

  local_mocked_bindings(
    get_cog = function(...) list(
      com = data.frame(COM = "01001", NCC_COM = "NCC_01001", DEP = "01"),
      dep = data.frame(DEP = "01", NCC_DEP = "NCC_01", REG = "01"),
      reg = data.frame(REG = "01", NCC_REG = "NCC_01")
    )
  )

  res <- get_parca_etalab(idu)

  expect_length(urls, 3)
  expect_true(all(grepl("/communes/0100", urls)))
})

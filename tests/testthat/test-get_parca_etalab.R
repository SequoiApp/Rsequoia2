make_fake_parcelles <- function(idu, commune = substr(idu, 1, 5), contenance = 10) {
  x <- Rsequoia2:::seq_poly[rep(1L, length(idu)), ]

  x$id <- idu
  x$commune <- commune
  x$prefixe <- substr(idu, 6, 8)
  x$section <- substr(idu, 9, 10)
  x$numero <- substr(idu, 11, 14)
  x$contenance <- contenance

  x
}


make_fake_cog <- function(insee = "01001") {
  dep <- substr(insee, 1, 2)

  list(
    com = data.frame(
      COM = insee,
      NCC_COM = paste0("NCC_", insee),
      DEP = dep
    ),
    dep = data.frame(
      DEP = unique(dep),
      NCC_DEP = paste0("NCC_", unique(dep)),
      REG = "01"
    ),
    reg = data.frame(
      REG = "01",
      NCC_REG = "NCC_01"
    )
  )
}


test_that("get_parca_etalab() aborts on empty idu", {
  expect_error(get_parca_etalab(character()), "idu.*non-empty")
  expect_error(get_parca_etalab(NULL), "idu.*non-empty")
  expect_error(get_parca_etalab(numeric()), "idu.*non-empty")

  expect_error(get_parca_etalab(Rsequoia2:::seq_poly), "must be")
  expect_error(get_parca_etalab(list("aaa")), "must be")
})


test_that("get_parca_etalab() aborts when all idu are invalid", {
  idu <- c("352890000A0145", "352890000A0147")

  local_mocked_bindings(
    read_etalab = function(insee, layer) {
      expect_equal(layer, "parcelles")
      make_fake_parcelles("999990000A0001", commune = "99999")
    }
  )

  expect_error(get_parca_etalab(idu), "Invalid idu detected:")
})


test_that("get_parca_etalab() aborts when any idu is invalid", {
  idu <- c("352890000A0145", "352890000A0147")

  local_mocked_bindings(
    read_etalab = function(insee, layer) {
      expect_equal(layer, "parcelles")
      make_fake_parcelles("352890000A0145", commune = "35289")
    }
  )

  expect_error(
    get_parca_etalab(idu),
    "Invalid idu detected:.*352890000A0147"
  )
})


test_that("get_parca_etalab() de-duplicates idu in output", {
  idu <- c("010010000A0001", "010010000A0001")

  calls <- 0

  local_mocked_bindings(
    read_etalab = function(insee, layer) {
      calls <<- calls + 1
      expect_equal(insee, "01001")
      expect_equal(layer, "parcelles")
      make_fake_parcelles("010010000A0001")
    },
    get_cog = function(...) make_fake_cog("01001")
  )

  res <- get_parca_etalab(idu)

  expect_equal(calls, 1)
  expect_equal(nrow(res), 1)
})


test_that("get_parca_etalab() pads cadastral fields", {
  idu <- "010010000A0001"

  fake <- make_fake_parcelles(
    idu = idu,
    commune = "1001"
  )

  fake$prefixe <- "1"
  fake$section <- "A"
  fake$numero <- "1"

  local_mocked_bindings(
    read_etalab = function(insee, layer) fake,
    get_cog = function(...) make_fake_cog("01001")
  )

  res <- get_parca_etalab(idu)

  f <- \(x) seq_field(x)$name

  expect_equal(res[[f("prefix")]], "001")
  expect_equal(res[[f("section")]], "0A")
  expect_equal(res[[f("number")]], "0001")
  expect_equal(res[[f("insee")]], "01001")
})


test_that("get_parca_etalab() adds source", {
  idu <- "010010000A0001"

  local_mocked_bindings(
    read_etalab = function(insee, layer) make_fake_parcelles(idu),
    get_cog = function(...) make_fake_cog("01001")
  )

  res <- get_parca_etalab(idu)
  source <- seq_field("source")$name

  expect_s3_class(res, "sf")
  expect_equal(res[[source]], "etalab")
})


test_that("get_parca_etalab() converts contenance to hectares", {
  idu <- "010010000A0001"

  local_mocked_bindings(
    read_etalab = function(insee, layer) {
      make_fake_parcelles(idu, contenance = 10)
    },
    get_cog = function(...) make_fake_cog("01001")
  )

  res <- get_parca_etalab(idu)
  cad_area <- seq_field("cad_area")$name

  expect_s3_class(res, "sf")
  expect_equal(res[[cad_area]], 0.001)
})


test_that("get_parca_etalab() requests unique communes from read_etalab()", {
  idu <- c(
    "010010000A0001",
    "010020000A0001",
    "010030000A0001",
    "010030000A0001"
  )

  requested_insee <- NULL
  requested_layer <- NULL

  local_mocked_bindings(
    read_etalab = function(insee, layer) {
      requested_insee <<- insee
      requested_layer <<- layer

      make_fake_parcelles(
        idu = unique(idu),
        commune = substr(unique(idu), 1, 5)
      )
    },
    get_cog = function(...) make_fake_cog(c("01001", "01002", "01003"))
  )

  res <- get_parca_etalab(idu)

  expect_equal(requested_layer, "parcelles")
  expect_equal(requested_insee, c("01001", "01002", "01003"))
  expect_equal(nrow(res), 3)
})

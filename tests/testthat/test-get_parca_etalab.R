test_that("get_parca_etalab() abort when all idu are invalid", {

  idu <- c("352890000A0145", "352890000A0147")
  local_mocked_bindings(
    read_sf = function(...) data.frame(
      id = "other_idu",
      prefixe = "000",
      section = "0A",
      numero = "0001"
    )
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
    )
  )

  expect_error(get_parca_etalab(idu), "Invalid idu detected:.*352890000A0147")

})

test_that("get_parca_etalab() add source", {

  poly <- sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))) |>
    sf::st_sfc(crs = 2154)

  idu <- c("352890000A0145")
  fake_parca_etalab <- sf::st_sf(
    id = idu,
    prefixe = "000",
    section = "0A",
    numero = "0001",
    geometry = poly
  )

  local_mocked_bindings(
    read_sf = function(...) fake_parca_etalab
  )

  res <- get_parca_etalab(idu)
  source <- seq_field("source")$name

  expect_s3_class(res, "sf")
  expect_all_equal(res[[source]], "etalab")

})

test_that("get_parca_etalab() deduplicates idu input", {

  idu <- c("352890000A0145", "352890000A0145")

  fake <- sf::st_sf(
    id = "352890000A0145",
    prefixe = "0",
    section = "A",
    numero = "1",
    geometry = sf::st_sfc(sf::st_point(c(0,0)), crs = 2154)
  )

  local_mocked_bindings(read_sf = function(...) fake)

  res <- get_parca_etalab(idu)

  expect_equal(nrow(res), 1)
})

test_that("get_parca_etalab() queries one URL per commune", {

  idu <- c("352890000A0145", "352900000A0001")

  calls <- 0
  local_mocked_bindings(
    read_sf = function(...) {
      calls <<- calls + 1
      sf::st_sf(
        id = c("352890000A0145", "352900000A0001")[calls],
        prefixe = "000",
        section = "0A",
        numero = "0001",
        geometry = sf::st_sfc(sf::st_point(c(0,0)), crs = 2154)
      )
    }
  )

  res <- get_parca_etalab(idu)

  expect_equal(calls, 2)
})

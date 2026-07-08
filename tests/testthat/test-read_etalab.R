test_that("read_etalab() reads one URL per unique commune", {
  urls <- character()

  local_mocked_bindings(
    read_sf = function(x, ...) {
      urls <<- c(urls, x)

      insee <- sub(".*/communes/([^/]+)/geojson/.*", "\\1", x)

      Rsequoia2:::seq_poly |>
        transform(
          id = paste0(insee, "000A0001"),
          commune = insee,
          prefixe = "000",
          section = "0A",
          numero = "0001",
          contenance = 10
        )
    },
    .package = "sf"
  )

  res <- read_etalab(
    c("01001", "01001", "01002"),
    layer = "parcelles"
  )

  expect_s3_class(res, "sf")
  expect_equal(length(urls), 2)

  expect_equal(
    urls,
    c(
      "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/01001/geojson/parcelles",
      "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/01002/geojson/parcelles"
    )
  )
})


test_that("read_etalab() supports lieux_dits layer", {
  urls <- character()

  local_mocked_bindings(
    read_sf = function(x, ...) {
      urls <<- c(urls, x)

      insee <- sub(".*/communes/([^/]+)/geojson/.*", "\\1", x)

      Rsequoia2:::seq_poly |>
        transform(
          id = paste0(insee, "_LD"),
          nom = paste0("Lieu dit ", insee)
        )
    },
    .package = "sf"
  )

  res <- read_etalab("01001", layer = "lieux_dits")

  expect_s3_class(res, "sf")
  expect_equal(length(urls), 1)

  expect_equal(
    urls,
    "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/01001/geojson/lieux_dits"
  )
})


test_that("read_etalab() rejects invalid layer", {
  expect_error(
    read_etalab("01001", layer = "invalid"),
    "should be one of"
  )
})

test_that("get_patrimony() validates x", {
  expect_error(get_patrimony(42), "x.*sf")
  expect_error(get_patrimony("a"), "x.*sf")
})

test_that("get_patrimony() validates type", {
  x <- sf::st_sfc(sf::st_point(c(0,0)), crs = 4326)

  expect_error(get_patrimony(x, key = "bad_key"), "available layers")
  expect_error(get_patrimony(x, key = c("key1", "key2")), "must contain exactly one element")
})

test_that("get_patrimony() works (local only)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  x <- sf::st_sf(sf::st_sfc(sf::st_point(c(7.410821, 48.854160)), crs = 4326))

  immh <- get_patrimony(x, key = "immh", buffer = 500)

  expect_s3_class(immh, "sf")
  expect_true(nrow(immh) >= 1)
  expect_equal(sf::st_crs(immh), sf::st_crs(2154))

})

test_that("get_patrimony() works offline", {

  mock_immh <- list(
    idTigre = 38881L,
    idApp = "IXSRD6",
    appTigre = 1L,
    type = "Immeuble",
    parcelle = NA_character_,
    appelation = "Eglise protestante",
    categorie = "architecture religieuse",
    localisati = "67524|Weiterswiller|rue Principale",
    ressource = "https://www.pop.culture.gouv.fr/notice/merimee/PA00085222",
    evenement = "classement le 26/09/1921",
    legende = "Class\U{E9}",
    precision = "RGE",
    statut = NA_character_,
    maj = "2019-4-9",
    region = "44",
    departemen = "67",
    commune = "Weiterswiller",
    geometry = list(
      list(
        matrix(
          c(
            1023737.071896255, 1023736.789847665, 1023736.250873785, 1023736.591693575,
            1023737.110690505, 1023741.991484005, 1023742.3825042151, 1023742.7617746448,
            1023742.370754435, 1023742.740713495, 1023743.633064645, 1023744.2618562449,
            1023743.349550575, 1023743.8173525851, 1023744.865592815, 1023744.303904725,
            1023743.255664485, 1023738.404713395, 1023738.4027389651, 1023723.130109855,
            1023722.189119725, 1023737.071896255, 6870484.645095841, 6870488.18563848,
            6870488.63683633, 6870489.0060622, 6870488.56490892, 6870488.97394694,
            6870489.43305787, 6870489.112215022, 6870488.653104091, 6870484.182387901,
            6870485.240358641, 6870484.708961281, 6870483.67103481, 6870478.2401237395,
            6870477.46779139, 6870476.709071111, 6870477.481403461, 6870477.00230032,
            6870476.12232724, 6870474.77662946, 6870483.23852474, 6870484.645095841
          ),
          nrow = 22L,
          ncol = 2L
        )
      ) |>
        structure(class = c("XY", "POLYGON", "sfg"))
    ) |>
      structure(
        n_empty = 0L,
        crs = list(
          input = "EPSG:2154",
          wkt = 'PROJCRS["RGF93 v1 / Lambert-93",\n    BASEGEOGCRS["RGF93 v1",\n        DATUM["Reseau Geodesique Francais 1993 v1",\n            ELLIPSOID["GRS 1980",6378137,298.257222101,\n                LENGTHUNIT["metre",1]]],\n        PRIMEM["Greenwich",0,\n            ANGLEUNIT["degree",0.0174532925199433]],\n        ID["EPSG",4171]],\n    CONVERSION["Lambert-93",\n        METHOD["Lambert Conic Conformal (2SP)",\n            ID["EPSG",9802]],\n        PARAMETER["Latitude of false origin",46.5,\n            ANGLEUNIT["degree",0.0174532925199433],\n            ID["EPSG",8821]],\n        PARAMETER["Longitude of false origin",3,\n            ANGLEUNIT["degree",0.0174532925199433],\n            ID["EPSG",8822]],\n        PARAMETER["Latitude of 1st standard parallel",49,\n            ANGLEUNIT["degree",0.0174532925199433],\n            ID["EPSG",8823]],\n        PARAMETER["Latitude of 2nd standard parallel",44,\n            ANGLEUNIT["degree",0.0174532925199433],\n            ID["EPSG",8824]],\n        PARAMETER["Easting at false origin",700000,\n            LENGTHUNIT["metre",1],\n            ID["EPSG",8826]],\n        PARAMETER["Northing at false origin",6600000,\n            LENGTHUNIT["metre",1],\n            ID["EPSG",8827]]],\n    CS[Cartesian,2],\n        AXIS["easting (X)",east,\n            ORDER[1],\n            LENGTHUNIT["metre",1]],\n        AXIS["northing (Y)",north,\n            ORDER[2],\n            LENGTHUNIT["metre",1]],\n    USAGE[\n        SCOPE["Engineering survey, topographic mapping."],\n        AREA["France - onshore and offshore, mainland and Corsica (France m\U{E9}tropolitaine including Corsica)."],\n        BBOX[41.15,-9.86,51.56,10.38]],\n    ID["EPSG",2154]]'
        ) |>
          structure(class = "crs"),
        class = c("sfc_POLYGON", "sfc"),
        precision = 0,
        bbox = c(
          xmin = 1023722.189119725,
          ymin = 6870474.77662946,
          xmax = 1023744.865592815,
          ymax = 6870489.43305787
        ) |>
          structure(class = "bbox")
      )
  ) |>
    structure(
      row.names = c(NA, -1L),
      class = c("sf", "data.frame"),
      sf_column = "geometry",
      agr = factor(
        c(
          idTigre = NA_character_,
          idApp = NA_character_,
          appTigre = NA_character_,
          type = NA_character_,
          parcelle = NA_character_,
          appelation = NA_character_,
          categorie = NA_character_,
          localisati = NA_character_,
          ressource = NA_character_,
          evenement = NA_character_,
          legende = NA_character_,
          precision = NA_character_,
          statut = NA_character_,
          maj = NA_character_,
          region = NA_character_,
          departemen = NA_character_,
          commune = NA_character_
        ),
        levels = c("constant", "aggregate", "identity")
      )
    )

  testthat::local_mocked_bindings(
    get_heritage = function(...) mock_immh,
    .package = "frheritage"
  )

  x <- sf::st_sf(sf::st_sfc(sf::st_point(c(7.410821, 48.854160)), crs = 4326))

  immh <- get_patrimony(x, key = "immh", buffer = 500)

  expect_s3_class(immh, "sf")
  expect_true(nrow(immh) >= 1)
  expect_equal(sf::st_crs(immh), sf::st_crs(2154))

})

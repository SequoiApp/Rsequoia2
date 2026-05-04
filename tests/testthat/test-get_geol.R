test_that("get_geol() reads and filters CarHab geology", {

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0),
        c(100, 0),
        c(100, 100),
        c(0, 100),
        c(0, 0)
      ))),
      crs = 2154
    )
  )

  geol <- sf::st_as_sf(
    data.frame(id = c(1, 2)),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(10, 10),
        c(20, 10),
        c(20, 20),
        c(10, 20),
        c(10, 10)
      ))),
      sf::st_polygon(list(rbind(
        c(1000, 1000),
        c(1100, 1000),
        c(1100, 1100),
        c(1000, 1100),
        c(1000, 1000)
      ))),
      crs = 2154
    )
  )

  tracker <- new.env(parent = emptyenv())
  tracker$key <- NULL

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, predicate) {
      data.frame(code_insee = "29")
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    download_carhab = function(dep, cache = NULL, verbose = FALSE, overwrite = FALSE) {
      tracker$key <- "carhab"
      stats::setNames("fake_carhab.zip", dep)
    },
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    archive = function(file) {
      data.frame(path = "folder/CarHab_fake.shp")
    },
    .package = "archive"
  )

  testthat::local_mocked_bindings(
    read_sf = function(dsn, ...) {
      expect_match(dsn, "CarHab_fake\\.shp$")
      geol
    },
    .package = "sf"
  )

  out <- get_geol(
    x = x,
    key = "carhab",
    verbose = FALSE
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(out$id, 1)
  expect_equal(tracker$key, "carhab")
})


test_that("get_geol() reads and filters BDCharm50 geology", {

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0),
        c(100, 0),
        c(100, 100),
        c(0, 100),
        c(0, 0)
      ))),
      crs = 2154
    )
  )

  geol <- sf::st_as_sf(
    data.frame(id = c(1, 2)),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(10, 10),
        c(20, 10),
        c(20, 20),
        c(10, 20),
        c(10, 10)
      ))),
      sf::st_polygon(list(rbind(
        c(1000, 1000),
        c(1100, 1000),
        c(1100, 1100),
        c(1000, 1100),
        c(1000, 1000)
      ))),
      crs = 2154
    )
  )

  tracker <- new.env(parent = emptyenv())
  tracker$key <- NULL

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, predicate) {
      data.frame(code_insee = "29")
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    download_bdcharm50 = function(dep, cache = NULL, verbose = FALSE, overwrite = FALSE) {
      tracker$key <- "bdcharm50"
      stats::setNames("fake_bdcharm50.zip", dep)
    },
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    archive = function(file) {
      data.frame(path = "folder/S_FGEOL_fake.shp")
    },
    .package = "archive"
  )

  testthat::local_mocked_bindings(
    read_sf = function(dsn, ...) {
      expect_match(dsn, "S_FGEOL_fake\\.shp$")
      geol
    },
    .package = "sf"
  )

  out <- get_geol(
    x = x,
    key = "bdcharm50",
    verbose = FALSE
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(out$id, 1)
  expect_equal(tracker$key, "bdcharm50")
})


test_that("get_geol() errors when x is not sf or sfc", {

  expect_error(
    get_geol(x = data.frame(a = 1), key = "carhab"),
    "must be of class"
  )
})


test_that("get_geol() errors on invalid key", {

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  expect_error(
    get_geol(x = x, key = "bad_key"),
    "'arg' should be one of"
  )
})


test_that("get_geol() errors when no shapefile is found in archive", {

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, predicate) {
      data.frame(code_insee = "29")
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    download_carhab = function(dep, cache = NULL, verbose = FALSE, overwrite = FALSE) {
      stats::setNames("fake_carhab.zip", dep)
    },
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    archive = function(file) {
      data.frame(path = "folder/not_geology.txt")
    },
    .package = "archive"
  )

  expect_error(
    get_geol(x = x, key = "carhab", verbose = FALSE),
    "No geology shapefile found"
  )
})


test_that("get_geol() errors when several shapefiles match in archive", {

  x <- sf::st_as_sf(
    data.frame(id = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  testthat::local_mocked_bindings(
    get_wfs = function(x, layer, predicate) {
      data.frame(code_insee = "29")
    },
    .package = "happign"
  )

  testthat::local_mocked_bindings(
    get_cog = function(...) {
      list(dep = data.frame(DEP = "29", NCC_DEP = "FINISTERE"))
    },
    download_carhab = function(dep, cache = NULL, verbose = FALSE, overwrite = FALSE) {
      stats::setNames("fake_carhab.zip", dep)
    },
    .package = "Rsequoia2"
  )

  testthat::local_mocked_bindings(
    archive = function(file) {
      data.frame(path = c(
        "folder/CarHab_fake_1.shp",
        "folder/CarHab_fake_2.shp"
      ))
    },
    .package = "archive"
  )

  expect_error(
    get_geol(x = x, key = "carhab", verbose = FALSE),
    "Several geology shapefiles found"
  )
})

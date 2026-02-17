fake_brgm_zip <- function(dep, cache){

  fake_sf <- sf::st_sf(
    data.frame(ID = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  # Write shapefile to temporary directory
  sf_path <- tempfile()
  dir.create(sf_path)
  on.exit(unlink(sf_path, recursive = TRUE), add = TRUE)
  sf::write_sf(fake_sf, file.path(sf_path, "S_FGEOL.shp"), quiet = TRUE)
  base::writeLines("", file.path(sf_path, "S_FGEOL.qml"))

  # Create ZIP (base R)
  zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
  zip_path <- file.path(cache, zip_name)
  utils::zip(
    zipfile = zip_path,
    files = list.files(sf_path, full.names = TRUE),
    flags = c("-j", "-q")
  )

  return(zip_path)
}

test_that("get_brgm() works for one dep", {

  cache <- file.path(tempdir(), "brgm")
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    download_brgm = function(dep, ...) fake_brgm_zip(dep, cache = cache)
  )

  brgm <- get_brgm(deps = 29, source = "bdcharm50", cache = cache, verbose = FALSE)

  expect_s3_class(brgm, "sf")
  expect_shape(brgm, dim = c(1, 2))
})

test_that("get_brgm() works for multiple dep", {

  cache <- file.path(tempdir(), "brgm")
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    download_brgm = function(dep, ...) fake_brgm_zip(dep, cache = cache)
  )

  brgm <- get_brgm(deps = 1:2, source = "bdcharm50", cache = cache, verbose = FALSE)

  expect_s3_class(brgm, "sf")
  expect_shape(brgm, dim = c(2, 2))
})

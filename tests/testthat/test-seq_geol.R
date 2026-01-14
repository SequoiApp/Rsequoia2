fake_brgm_zip <- function(dep, source, cache){

  fake_sf <- sf::st_sf(
    data.frame(ID = 1),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  # Write shapefile to temporary directory
  sf_path <- tempfile()
  on.exit(unlink(sf_path, recursive = T))
  dir.create(sf_path)

  if (source == "carhab"){
    all_dep <- happign::dep_2025
    dep_name <- all_dep[all_dep$DEP == dep, c("DEP", "NCC_DEP")]
    zip_name <- sprintf("CARHAB_%s.zip", paste(dep_name, collapse = "_"))
    sf::write_sf(fake_sf, file.path(sf_path, "CarHab.shp"), quiet = TRUE)
  }else{
    zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
    sf::write_sf(fake_sf, file.path(sf_path, "S_FGEOL.shp"), quiet = TRUE)
    base::writeLines("", file.path(sf_path, "S_FGEOL.qml"))
  }

  zip_path <- file.path(cache, zip_name)
  utils::zip(
    zipfile = zip_path,
    files = list.files(sf_path, full.names = TRUE),
    flags = c("-j", "-q")
  )

  return(zip_path)
}

test_that("seq_geol() works for one dep", {
  seq_cache <- file.path(tempdir(), "seq")
  dir.create(seq_cache)
  on.exit(unlink(seq_cache, recursive = TRUE, force = TRUE), add = TRUE)

  brgm_cache <- file.path(tempdir(), "brgm")
  dir.create(brgm_cache)
  on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

  m <- fake_matrice(id = "TEST")
  m_path <- file.path(seq_cache, "ECKMUHL_matrice.xlsx")
  openxlsx2::write_xlsx(m, m_path)

  p <- fake_parca(dep = 29)
  p_path <- seq_write(p, "parca", dirname = seq_cache)

  bdcharm50_29 <- fake_brgm_zip(29, source = "bdcharm50", cache = brgm_cache)
  carhab_29 <- fake_brgm_zip(29, source = "carhab", cache = brgm_cache)
  geol <- seq_geol(dirname = seq_cache, cache = brgm_cache, verbose = FALSE)

  expect_length(geol, 2)
  expect_length(list.files(seq_cache, pattern = "\\.qml$"), 1)
})



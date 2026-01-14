fake_brgm_zip <- function(dep, source, cache){

  # Write shapefile to temporary directory
  sf_path <- tempfile()
  on.exit(unlink(sf_path, recursive = T))
  dir.create(sf_path)

  if (source == "carhab"){
    all_dep <- happign::dep_2025
    dep_name <- all_dep[all_dep$DEP == dep, c("DEP", "NCC_DEP")]
    zip_name <- sprintf("CARHAB_%s.zip", paste(dep_name, collapse = "_"))
    sf::write_sf(Rsequoia2:::seq_poly, file.path(sf_path, "CarHab.shp"), quiet = TRUE)
  }else{
    zip_name <- sprintf("GEO050K_HARM_%s.zip", pad_left(dep, 3))
    sf::write_sf(Rsequoia2:::seq_poly, file.path(sf_path, "S_FGEOL.shp"), quiet = TRUE)
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

  with_seq_cache({
    brgm_cache <- file.path(tempdir(), "brgm")
    dir.create(brgm_cache)
    on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

    bdcharm50_29 <- fake_brgm_zip(29, source = "bdcharm50", cache = brgm_cache)
    carhab_29 <- fake_brgm_zip(29, source = "carhab", cache = brgm_cache)

    local_mocked_bindings(
      st_intersects = function(...) TRUE,
      .package = "sf"
    )

    paths <- seq_geol(dirname = seq_cache, cache = brgm_cache, verbose = FALSE, overwrite = TRUE)

    brgm <- lapply(paths, read_sf)

    identifier <- seq_field("identifier")$name
    expect_all_true(vapply(brgm, \(x) identifier %in% names(x), TRUE))

    expect_length(paths, 2)

    brgm_dir <- get_path("v.sol.carhab.poly") |> dirname()
    expect_length(list.files(file.path(seq_cache, brgm_dir), pattern = "\\.qml$"), 1)
  })
})



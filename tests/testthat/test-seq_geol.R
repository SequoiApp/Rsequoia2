mchtest_that("seq_geol() works for one dep", {

  with_seq_cache({
    brgm_cache <- file.path(tempdir(), "brgm")
    dir.create(brgm_cache)
    on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

    zip_name <- "GEO050K_HARM_029.zip"
    base::writeLines("", file.path(brgm_cache, "S_FGEOL.qml"))

    zip_path <- file.path(brgm_cache, zip_name)
    utils::zip(
      zipfile = zip_path,
      files = list.files(brgm_cache, full.names = TRUE),
      flags = c("-j", "-q")
    )

    tracker <- list()
    local_mocked_bindings(
      get_brgm = function(dep, source, ...){
        tracker$source <<- c(tracker$source, source)
        return(p)
      }
    )

    paths <- seq_geol(dirname = seq_cache, cache = brgm_cache, verbose = FALSE, overwrite = TRUE)

    expect_length(paths, 2)

    layer_info <- seq_layer("v.sol.carhab.poly")
    path <- layer_info$path
    expect_length(list.files(file.path(seq_cache, path), pattern = "\\.qml$"), 1)

    expect_all_true(tracker$source %in% c("carhab", "bdcharm50"))
  })

})

test_that("seq_geol() downloads both geology layers by default", {

  with_seq_cache({

    brgm_cache <- file.path(tempdir(), "brgm")
    dir.create(brgm_cache)
    on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

    # ---- Fake QML inside ZIP ----
    zip_name <- "GEO050K_HARM_029.zip"
    qml_file <- file.path(brgm_cache, "S_FGEOL_fake.qml")
    writeLines("qml", qml_file)

    zip_path <- file.path(brgm_cache, zip_name)
    capture.output(
      utils::zip(zipfile = zip_path, files = qml_file, flags = c("-j", "-q")),
      file = NULL
    )

    # ---- Mock get_brgm ----
    tracker <- list(key = character())

    local_mocked_bindings(
      get_brgm = function(dep, key, ...) {
        tracker$key <<- c(tracker$key, key)
        return(p)  # p should be your fake sf test object
      }
    )

    paths <- seq_geol(
      dirname = seq_cache,
      key = NULL,
      cache = brgm_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    # ---- Assertions ----
    expect_named(paths, c("v.sol.carhab.poly", "v.sol.bdcharm50.poly"))
    expect_equal(sort(unique(tracker$key)),
                 sort(c("carhab", "bdcharm50")))

    # QML must exist for bdcharm50
    bd_path <- paths[["v.sol.bdcharm50.poly"]]
    expect_true(file.exists(sub("\\.gpkg$", ".qml", bd_path)))
  })
})

test_that("seq_geol() respects key argument", {

  with_seq_cache({

    tracker <- list(key = character())

    local_mocked_bindings(
      get_brgm = function(dep, key, ...) {
        tracker$key <<- c(tracker$key, key)
        return(p)
      }
    )

    paths <- seq_geol(
      key = "carhab",
      dirname = seq_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_named(paths, "v.sol.carhab.poly")
    expect_equal(unique(tracker$key), "carhab")
  })
})

test_that("seq_geol() errors on invalid key", {
  expect_error(
    seq_geol(key = "invalid_layer", verbose = FALSE),
    "Valid keys are defined in"
  )
})

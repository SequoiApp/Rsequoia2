test_that("seq_geol() downloads both geology layers by default", {
  with_seq_cache({

    # Fake BRGM cache + QML archive
    brgm_cache <- tempfile("brgm_")
    dir.create(brgm_cache)
    on.exit(unlink(brgm_cache, recursive = TRUE, force = TRUE), add = TRUE)

    qml_file <- file.path(brgm_cache, "S_FGEOL_fake.qml")
    writeLines("qml", qml_file)

    zip_path <- file.path(brgm_cache, "GEO050K_HARM_029.zip")

    capture.output(
      utils::zip(
        zipfile = zip_path,
        files = qml_file,
        flags = c("-j", "-q")
      ),
      file = NULL
    )

    # Track requested geology datasets
    tracker <- list(key = character())

    local_mocked_bindings(
      # I need to return parca to ensure intersectiuon doesn't return no data
      get_geol = function(x, key, ...) {
        tracker$key <<- c(tracker$key, key)
         p
      },
      download_bdcharm50 = function(...) {
        zip_path
      }
    )

    local_mocked_bindings(
      get_wfs = function(...) {
        data.frame(code_insee = "29")
      },
      .package = "happign"
    )

    paths <- seq_geol(
      dirname = seq_cache,
      cache = brgm_cache,
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_named(paths, c("carhab", "bdcharm50"))

    expect_equal(
      sort(unique(tracker$key)),
      sort(c("carhab", "bdcharm50"))
    )

    expect_all_true(file.exists(unlist(paths)))

    # BD Charm QML must also be written
    qml_path <- paste0(
      tools::file_path_sans_ext(paths[["bdcharm50"]]),
      ".qml"
    )

    expect_true(file.exists(qml_path))
  })
})


test_that("seq_geol() respects key argument", {
  with_seq_cache({

    tracker <- list(key = character())

    local_mocked_bindings(
      get_geol = function(x, key, ...) {
        tracker$key <<- c(tracker$key, key)
         p
      }
    )

    paths <- seq_geol(
      dirname = seq_cache,
      key = "carhab",
      verbose = FALSE,
      overwrite = TRUE
    )

    expect_named(paths, "carhab")
    expect_equal(unique(tracker$key), "carhab")
    expect_true(file.exists(paths[["carhab"]]))
  })
})


test_that("seq_geol() rejects invalid key", {
  with_seq_cache({
    expect_error(
      seq_geol(dirname = seq_cache, key = "invalid", verbose = FALSE),
      "Invalid"
    )
  })
})


test_that("seq_geol() skips empty geology layers", {
  with_seq_cache({

    local_mocked_bindings(
      get_geol = function(...) NULL
    )

    paths <- seq_geol(dirname = seq_cache, verbose = FALSE)

    expect_length(paths, 0)
  })
})


test_that("seq_geol() adds project identifier", {
  with_seq_cache({

    local_mocked_bindings(
      get_geol = function(...) p
    )

    paths <- seq_geol(
      dirname = seq_cache,
      key = "carhab",
      verbose = FALSE,
      overwrite = TRUE
    )

    geol <- sf::read_sf(paths[["carhab"]])

    identifier <- seq_field("identifier")$name

    expect_true(identifier %in% names(geol))
    expect_identical(
      unique(geol[[identifier]]),
      "ECKMUHL"
    )
  })
})
